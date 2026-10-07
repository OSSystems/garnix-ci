{-# LANGUAGE OverloadedRecordDot #-}

module Garnix.API.ModulesSpec where

import Control.Lens
import Cradle qualified
import Data.Aeson.KeyMap qualified as JSON.KeyMap
import Data.Aeson.Lens
import Data.Aeson.Types qualified as JSON
import Data.Row ((.+), (.-), (.==))
import Database.PostgreSQL.Typed
import Garnix.DB qualified as DB
import Garnix.DB.ModuleValues qualified as ModuleValues
import Garnix.Duration
import Garnix.Monad
import Garnix.Prelude
import Garnix.TestHelpers hiding (testUser)
import Garnix.TestHelpers.GithubInterface qualified as GH
import Garnix.TestHelpers.Monad
import Garnix.TestHelpers.WithServer
import Garnix.Types hiding (context)
import Network.Wreq.Lens
import Test.HUnit (assertFailure)
import Test.Hspec hiding (shouldReturn, shouldThrow)

spec :: Spec
spec = inM . beforeM_ truncateDBM . aroundM_ suppressLogsWhenPassing . context "ModulesSpec" $ do
  context "GET /api/modules" $ do
    it "returns 401 Unauthorized when the user is not logged in" $ withServer $ \server -> do
      res <- server.get "/api/modules"
      res `shouldHaveStatusCode` 401

    it "returns 404 when there are no values in the db" $ withServer $ \server -> do
      void server.login
      res <- server.get "/api/modules"
      res `shouldHaveStatusCode` 404

    it "returns the values when they exist" $ withServer $ \server -> do
      user <- server.login
      values <- mkModuleValues ["test-module"]
      ModuleValues.update (userForgeLogin user) (values .- #modules)
      res <- assert200 $ server.get "/api/modules"
      res ^?! responseBody . _Value `shouldBeM` toJSON values

    it "returns the correct versions of modules even if there are newer versions available" $ withServer $ \server -> do
      user <- server.login
      oldVersion <- mkModuleValues ["test-module"]
      1 <-
        DB.pgExec
          [pgSQL|
            UPDATE modules
              SET enabled = false
              WHERE name = 'test-module'
          |]
      1 <-
        DB.pgExec
          [pgSQL|
            INSERT INTO modules
              (repo_user, repo_name, git_commit, schema, enabled, name)
            VALUES
              ('garnix-io', 'test-module-module', 'latest-hash-here', '{}', true, 'test-module')
          |]
      ModuleValues.update (userForgeLogin user) (oldVersion .- #modules)
      res <- assert200 $ server.get "/api/modules"
      res ^?! responseBody . _Value `shouldBeM` toJSON oldVersion

  context "PUT /api/modules" $ do
    it "returns 401 Unauthorized when the user is not logged in" $ withServer $ \server -> do
      payload <- toJSON <$> mkModuleValues ["test-module"]
      res <- server.put "/api/modules" payload
      res `shouldHaveStatusCode` 401

    it "returns 400 when the payload is not well-formed" $ withServer $ \server -> do
      void server.login
      res <- server.put "/api/modules" $ toJSON True
      res `shouldHaveStatusCode` 400

    it "returns the values when they exist" $ withServer $ \server -> do
      void server.login
      payload <- toJSON <$> mkModuleValues ["test-module"]
      void . assert200 $ server.put "/api/modules" payload
      res <- assert200 $ server.get "/api/modules"
      res ^?! responseBody . _Value `shouldBeM` payload

    it "removes missing module configs on subsequent pushes" $ withServer $ \server -> do
      void server.login
      initial <- toJSON <$> mkModuleValues ["foo-module", "bar-module"]
      void . assert200 $ server.put "/api/modules" initial
      payload <- toJSON <$> mkModuleValues ["foo-module"]
      void . assert200 $ server.put "/api/modules" payload
      res <- assert200 $ server.get "/api/modules"
      res ^?! responseBody . _Value . key "user_config" `shouldBeM` (payload ^?! _Value . key "user_config")
      res ^?! responseBody . _Value . key "modules" `shouldBeM` (initial ^?! _Value . key "modules")

  context "GET /api/modules/available" $ do
    it "returns the list of modules" $ withServer $ \server -> do
      void $ mkModuleValues ["test-module"]
      res <- assert200 $ server.get "/api/modules/available"
      res ^.. responseBody . key "modules" . _Array . traversed . key "name" `shouldBeM` ["test-module"]

    it "returns the latest version even when the user is using older modules" $ withServer $ \server -> do
      user <- server.login
      oldVersion <- mkModuleValues ["test-module"]
      1 <-
        DB.pgExec
          [pgSQL|
            UPDATE modules
              SET enabled = false
              WHERE name = 'test-module'
          |]
      1 <-
        DB.pgExec
          [pgSQL|
            INSERT INTO modules
              (repo_user, repo_name, git_commit, schema, enabled, name)
            VALUES
              ('garnix-io', 'test-module-module', 'latest-hash-here', '{}', true, 'test-module')
          |]
      ModuleValues.update (userForgeLogin user) (oldVersion .- #modules)
      res <- assert200 $ server.get "/api/modules/available"
      let expected =
            oldVersion
              .- #repo_user
              .- #repo_name
              .- #user_config
              & #modules . traversed . #git_commit .~ CommitHash "latest-hash-here"
      res ^?! responseBody . _Value `shouldBeM` toJSON expected

  context "POST /api/modules/run" $ do
    let emptyBody = toJSON ()
    it "returns 404 when not logged in" $ withServer $ \server -> do
      res <- server.post "/api/modules/run" emptyBody
      res `shouldHaveStatusCode` 401

    it "returns 404 when no module is configured" $ withServer $ \server -> do
      void server.login
      res <- server.post "/api/modules/run" emptyBody
      res `shouldHaveStatusCode` 404

    it "starts the build when everything is configured" $ withServer $ \server -> do
      user <- server.login
      payload <- toJSON <$> mkModuleValues ["test-module"]
      void $ assert200 $ server.put "/api/modules" payload
      res <- assert200 $ server.post "/api/modules/run" emptyBody
      waitFor (fromSeconds @Int 1) $ do
        builds <- DB.getBuilds user
        null builds `shouldBeM` False
      res
        ^?! responseBody
          . _Value
          `shouldBeM` [aesonQQ| {
        commit: "aaaa",
        branch: "main"
      }|]

  context "GET /api/modules/reset" $ do
    it "returns 401 when not logged in" $ withServer $ \server -> do
      res <- server.get "/api/modules/reset"
      res `shouldHaveStatusCode` 401

    it "returns 404 when no module is configured" $ withServer $ \server -> do
      void server.login
      res <- server.get "/api/modules/reset"
      res `shouldHaveStatusCode` 404

    it "returns 200 when everything is configured" $ withServer $ \server -> do
      void server.login
      payload <- toJSON <$> mkModuleValues ["test-module"]
      void $ assert200 $ server.put "/api/modules" payload
      res <- assert200 $ server.get "/api/modules/reset"
      res ^. responseHeader "Content-Disposition" `shouldBeM` "attachment; filename=\"flake.nix\""
      liftIO $ res ^. responseBody `shouldNotBe` ""

  context "POST /api/modules/eject" $ do
    let emptyBody = toJSON ()
    it "returns 401 when not logged in" $ withServer $ \server -> do
      res <- server.post "/api/modules/reset" emptyBody
      res `shouldHaveStatusCode` 401

    it "returns 404 when no module is configured" $ withServer $ \server -> do
      void server.login
      res <- server.post "/api/modules/reset" emptyBody
      res `shouldHaveStatusCode` 404

    it "returns 200 when everything is configured" $ withServer $ \server -> do
      void server.login
      payload <- toJSON <$> mkModuleValues ["test-module"]
      void $ assert200 $ server.put "/api/modules" payload
      void $ assert200 $ server.post "/api/modules/reset" emptyBody

  context "POST /api/modules/pull-request" $ do
    let postPullRequest server = server.post "/api/modules/pull-request" (toJSON ())
    it "returns 401 when not logged in" $ withServer $ \server -> do
      res <- postPullRequest server
      res `shouldHaveStatusCode` 401

    it "returns 404 when no module is configured" $ withServer $ \server -> do
      void server.login
      res <- postPullRequest server
      res `shouldHaveStatusCode` 404

    it "creates a PR with everything setup correctly" $ do
      GH.withFakeGithubInterface $ \ghState -> do
        GH.withLocalRepo ghState "owner" "repo" identity defaultCommitInfo GH.setupWithNoFlake $ \_eventInfo ->
          withServer $ \server -> do
            void server.login
            payload <- toJSON <$> mkModuleValues ["nodejs"]
            void $ assert200 $ server.put "/api/modules" payload
            res <- postPullRequest server
            res `shouldHaveStatusCode` 200

            Just (path, branch) <-
              GH.lookupRepo ghState "owner" "repo" >>= \case
                Nothing -> liftIO $ assertFailure "repo not found"
                Just repo -> pure $ do
                  path <- repo ^. #localPath
                  branch <- repo ^. #pullRequestBranch
                  pure (path, branch)
            commitHasFlakeNix <-
              Cradle.run
                $ Cradle.cmd "git"
                & Cradle.setWorkingDir path
                & Cradle.addArgs ["show", "--stat", getBranch branch]
                & Cradle.silenceStderr
            case commitHasFlakeNix of
              (Cradle.ExitFailure _, _) -> liftIO $ assertFailure "could not find git branch"
              (Cradle.ExitSuccess, Cradle.StdoutTrimmed stdout) -> do
                when (not $ any ("flake.nix" `isPrefixOf`) (tails $ cs stdout))
                  $ liftIO
                  $ assertFailure
                  $ "could not find flake.nix commit in head: "
                  <> cs stdout
                  <> cs path

            res
              ^?! responseBody
                . _Value
                `shouldBeM` [aesonQQ| {
                  url: "owner/repo/pulls/1"
                }|]

mkModuleValues :: [Text] -> M ModuleValues.GetRepoAndModuleValues
mkModuleValues moduleNames = do
  -- we use a real hash in order to allow `flake lock` to work;
  -- it is a real hash on the `garnix-io-nodejs-module` repo
  let nodejsModuleHash = "5974a179f249ec802145371e3a4658725a3eb900"
  forM_ moduleNames $ \moduleName -> do
    let moduleRepoName = moduleName <> "-module"
    void
      $ DB.pgExec
        [pgSQL|
          INSERT INTO modules
            (repo_user, repo_name, git_commit, schema, enabled, name)
          VALUES
            ('garnix-io', ${moduleRepoName}, ${nodejsModuleHash}, '{}', true, ${moduleName})
          ON CONFLICT DO NOTHING
        |]
  pure $ #repo_user .== Just "owner"
    .+ #repo_name .== Just "repo"
    .+ #user_config
      .== map
        ( \moduleName ->
            #module_name .== moduleName
              .+ #git_commit .== Just (CommitHash nodejsModuleHash)
              .+ #values .== ModuleValues.ModuleConfig (ModuleValues.NixIdentifier "hello") (ModuleValues.NixString "world")
        )
        moduleNames
    .+ #modules
      .== map
        ( \moduleName ->
            #name .== moduleName
              .+ #repo_user .== "garnix-io"
              .+ #repo_name .== GhRepoName (moduleName <> "-module")
              .+ #git_commit .== CommitHash nodejsModuleHash
              .+ #schema .== JSON.Object JSON.KeyMap.empty
              .+ #description .== Nothing
        )
        moduleNames
