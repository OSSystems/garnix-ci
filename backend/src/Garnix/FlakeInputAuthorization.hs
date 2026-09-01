{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Garnix.FlakeInputAuthorization
  ( checkAuthorization,
    -- exported for tests
    FlakeInput (..),
    GithubFlakeInput (..),
    _parseFlakeMetaData,
    _extractPrivateReposFromErrors,
  )
where

import Control.Lens.Regex.Text qualified as RE
import Cradle
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, Value, withObject, (.:))
import Data.Text qualified as T
import Garnix.Monad
import Garnix.NixConfig
import Garnix.Prelude
import Garnix.Sandbox
import Garnix.Types hiding (owner, repo)
import Network.URI
import System.Directory.Extra (canonicalizePath, doesPathExist)
import System.FilePath (isAbsolute)

-- | Throws if access should be denied.
-- Otherwise, returns the needed authorization to fetch all flake inputs.
checkAuthorization :: (HasCallStack) => FlakeDir -> RepoConfig -> CommitInfo -> M NixConfig
checkAuthorization flakeDir repoConfig commitInfo = do
  cacheDir <- getNixXdgCacheDir
  nixConfig <- view #userNixConfig
  curDir <- view #workingDir
  flakeDir' <- safeGetAbsoluteFlakeDir flakeDir
  (exitCode, StdoutTrimmed stdout, StderrRaw stderr) <-
    (>>= run)
      $ cmd "nix"
      & addArgs ["flake", "metadata", "--json", flakeDir']
      & addNixConfigEnvironment nixConfig
      & setWorkingDir curDir
      & pure
      & inNixSandbox [] (Just cacheDir)
  output <- case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure e -> do
      log Informational $ "'nix flake metadata --json' failed with stderr: " <> cs stderr
      case _extractPrivateReposFromErrors (cs stderr) of
        Nothing ->
          throw
            $ RunProcessError
              { command = "nix",
                arguments = ["flake", "metadata", "--json", cs flakeDir'],
                stdErr = cs stderr,
                stdOut = stdout,
                exitCode = e
              }
        Just privateInputs ->
          throw
            $ OtherError
            $ T.intercalate "\n"
            $ flip map privateInputs
            $ \input ->
              showPretty input <> " is private or doesn't exist (or your flake.lock file is outdated).\nIf it is private and you would like to use it, see https://garnix.io/docs/private_inputs."
  inputs <- aesonDecode "output of 'nix flake metadata --json'" _parseFlakeMetaData output
  githubInputs <- checkInputsAllowed curDir inputs
  let repoInfo' = commitInfo ^. repoInfo
  privateInputs <- filterM (\(GithubFlakeInput owner repo) -> not . isRepoPublic <$> getRepoPublicity (repoInfo' ^. installationAuth) owner repo) githubInputs
  selfRepoPublicity <- getRepoPublicity (repoInfo' ^. installationAuth) (repoInfo' ^. ghRepoOwner) (repoInfo' ^. ghRepoName)
  case privateInputs of
    [] -> pure $ NixConfig mempty
    _
      | isRepoPublic selfRepoPublicity -> do
          let skipPrivateInputChecks = repoConfig ^. skipPrivateInputsCheckForCollaborators
          unless skipPrivateInputChecks $ do
            throw
              $ OtherError
              $ "Public repository has private dependencies, which is not allowed. Private dependencies: "
              <> T.unwords (fmap showPretty privateInputs)
          pure $ githubAccessTokenNixConfig $ repoInfo' ^. ghToken
      | isJust $ commitInfo ^. prFromFork ->
          throw
            $ OtherError
              "Repository has private dependencies, but PR is from fork."
    _ -> do
      baseRepoCollaborators' <- getRepoCollaborators (repoInfo' ^. installationAuth) (repoInfo' ^. ghRepoOwner) (repoInfo' ^. ghRepoName) <?> "Getting repo collaborators"
      baseRepoCollaborators <- case baseRepoCollaborators' of
        RepoNotFound -> throw $ OtherError "checkAuthorization: base repo not found"
        GhCollaborators collaborators -> pure collaborators
      forM_ privateInputs $ \privateInput -> do
        when ((repoInfo' ^. ghRepoOwner) /= owner privateInput) $ do
          throw $ OtherError $ showPretty privateInput <> " is private or doesn't exist.\nIf it is private and you would like to use it, see https://garnix.io/docs/private_inputs."
        let skipPrivateInputChecks = repoConfig ^. skipPrivateInputsCheckForCollaborators
        unless skipPrivateInputChecks $ do
          thisInputCollaborators' <-
            getRepoCollaborators (repoInfo' ^. installationAuth) (repoInfo' ^. ghRepoOwner) (repo privateInput)
          thisInputCollaborators <- case thisInputCollaborators' of
            RepoNotFound -> throw $ OtherError $ "checkAuthorization: repo " <> (getGhLogin . getGhRepoOwner $ repoInfo' ^. ghRepoOwner) <> "/" <> getGhRepoName (repoInfo' ^. ghRepoName) <> " not found"
            GhCollaborators collaborators -> pure collaborators
          let missingUsers = filter (`notElem` thisInputCollaborators) baseRepoCollaborators
          unless (null missingUsers)
            $ throw
            $ OtherError
            $ "Aborting, as some collaborators of this repository "
            <> "don't have access to a required private dependency ("
            <> showPretty privateInput
            <> "). The users missing permissions are: "
            <> showPretty missingUsers
      pure $ githubAccessTokenNixConfig $ repoInfo' ^. ghToken

_extractPrivateReposFromErrors :: Text -> Maybe [Text]
_extractPrivateReposFromErrors s =
  case s ^.. [RE.regex|while fetching the input '(github:.*)'\n|] . RE.groups of
    [] -> Nothing
    matches -> Just $ flip map matches $ \case
      [match] -> match
      _ -> error "impossible: regex only has one match group"

checkInputsAllowed :: FilePath -> [FlakeInput] -> M [GithubFlakeInput]
checkInputsAllowed repoDir inputs = do
  maybes <- forM inputs $ \flakeInput -> case flakeInput of
    Github githubFlakeInput -> pure $ Just githubFlakeInput
    PathInput path -> do
      unlessM (pathInputIsOk repoDir path) $ do
        log Informational $ "disallowed flake input: " <> show (pretty flakeInput)
        throw $ OtherError $ "flake inputs of type 'path:' not allowed: " <> show (pretty flakeInput)
      pure Nothing
    FileInput url -> do
      case uriScheme <$> parseURI url of
        Just "http:" -> pure Nothing
        Just "https:" -> pure Nothing
        _ -> do
          log Informational $ "disallowed flake input: " <> show (pretty flakeInput)
          throw $ OtherError $ "flake input disallowed: " <> show (pretty flakeInput)
    RawRepoUrlInput url -> do
      case uriScheme <$> parseURI url of
        Just "http:" -> pure Nothing
        Just "https:" -> pure Nothing
        Just "ssh:" -> pure Nothing
        _ -> do
          log Informational $ "disallowed flake input: " <> show (pretty flakeInput)
          throw $ OtherError $ "flake input disallowed: " <> show (pretty flakeInput)
  pure $ catMaybes maybes
  where
    pathInputIsOk :: FilePath -> FilePath -> M Bool
    pathInputIsOk repoDir inputPath
      | isAbsolute inputPath = pure False
      | otherwise = do
          let combined = repoDir </> inputPath
          exists <- liftIO $ doesPathExist combined
          if not exists
            then pure False
            else do
              normalizedInputPath <- liftIO $ canonicalizePath combined
              pure $ (repoDir <> "/") `isPrefixOf` normalizedInputPath

data FlakeInput
  = Github GithubFlakeInput
  | PathInput FilePath
  | FileInput FilePath
  | RawRepoUrlInput String
  deriving stock (Show, Eq, Ord)

instance Pretty FlakeInput where
  pretty = \case
    Github input -> pretty input
    PathInput path -> "path:" <> pretty path
    FileInput url -> pretty url
    RawRepoUrlInput url -> pretty url

data GithubFlakeInput = GithubFlakeInput {owner :: GhRepoOwner, repo :: GhRepoName}
  deriving stock (Show, Eq, Ord)

instance Pretty GithubFlakeInput where
  pretty GithubFlakeInput {owner, repo} =
    "github:" <> pretty owner <> "/" <> pretty repo

_parseFlakeMetaData :: Value -> Parser [FlakeInput]
_parseFlakeMetaData json = do
  locks <- withObject "flake metadata" pure json >>= (.: "locks")
  nodes <- locks .: "nodes"
  rootKey <- locks .: "root"
  nodes :: KeyMap Value <-
    KeyMap.delete rootKey
      <$> parseJSON nodes
  fmap catMaybes
    $ forM (KeyMap.elems nodes)
    $ withObject "flake input node"
    $ \o -> do
      parseFlakeInput o "original"

parseFlakeInput :: KeyMap Value -> Text -> Parser (Maybe FlakeInput)
parseFlakeInput input key = do
  obj <- input .: fromString (cs key)
  typ :: Maybe Text <- obj .: "type"
  case typ of
    Just "indirect" -> do
      when
        (key == "locked")
        (fail "Type of locked inputs can't be \"indirect\".")
      parseFlakeInput input "locked"
    Just "github" ->
      Just . Github <$> (GithubFlakeInput <$> obj .: "owner" <*> obj .: "repo")
    Just "path" -> do
      Just . PathInput <$> obj .: "path"
    Just "file" -> do
      Just . FileInput <$> obj .: "url"
    Just "git" -> do
      Just . RawRepoUrlInput <$> obj .: "url"
    Just "hg" -> do
      Just . RawRepoUrlInput <$> obj .: "url"
    Just typ | typ `elem` otherBlessedTypes -> pure Nothing
    Just typ ->
      fail $ "unsupported flake input type: " <> cs typ
    Nothing ->
      fail "missing flake input type"

otherBlessedTypes :: [Text]
otherBlessedTypes = ["tarball", "gitlab", "sourcehut"]
