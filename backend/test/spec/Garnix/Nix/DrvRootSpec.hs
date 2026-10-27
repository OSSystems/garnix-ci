module Garnix.Nix.DrvRootSpec where

import Control.Lens
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Nix.DrvRoot
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.GithubInterface
import Garnix.TestHelpers.Monad
import Garnix.Types
import System.Directory (createDirectoryIfMissing, listDirectory)
import Test.Hspec

spec :: Spec
spec = inM
  $ aroundM_ suppressLogsWhenPassing
  $ beforeM_ truncateDBM
  $ describe "removeStaleDrvRoots"
  $ do
    it "keeps the roots of pending builds and drops every other" $ do
      withFakeGithubInterface $ \ghState -> do
        mkRepo ghState "owner" "repo" identity
        DB.newCommit (RepoId githubForge "owner" "repo") (defaultCommitInfo ^. commit)
        pending <- newBuild "pending"
        finished <- newBuild "finished"
        now <- liftIO getCurrentTime
        DB.reportBuildResultDB (finished & status ?~ Success & endTime ?~ now)
        dir <- view #drvRootsDir
        liftIO $ do
          createDirectoryIfMissing True dir
          forM_ [rootName pending, rootName finished, "not-a-build"] $ \name ->
            writeFile (dir </> name) ""
        removeStaleDrvRoots
        liftIO (listDirectory dir) `shouldReturnM` [rootName pending]
  where
    newBuild :: Text -> M Build
    newBuild name =
      DB.newBuildDB
        defaultCommitInfo
        (PackageInfo TypePackage (IsSystem X8664Linux) (PackageName name))
        "garnix-server-test"
        False

    rootName :: Build -> FilePath
    rootName build = cs $ toUrlPiece $ build ^. id
