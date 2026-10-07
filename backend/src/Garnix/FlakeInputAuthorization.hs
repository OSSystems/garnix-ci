{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Garnix.FlakeInputAuthorization
  ( checkAuthorization,
    InputAuthorization (..),
    InstanceInput (..),
    RejectedInput (..),
    GithubVisibility (..),
    -- exported for tests
    FlakeInput (..),
    GithubFlakeInput (..),
    _parseFlakeMetaData,
    _parseGiteaInputs,
    _privateInputCredentials,
    _githubVisibility,
    _extractPrivateReposFromErrors,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Control.Lens.Regex.Text qualified as RE
import Cradle
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (key, _Bool, _String)
import Data.Aeson.Types (Parser, Value, withObject, (.:), (.:?))
import Data.Containers.ListUtils (nubOrd)
import Data.Either (lefts, rights)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Garnix.Forge.Gitea (giteaInputHost, giteaInputRepo, giteaNetRcEntry)
import Garnix.Monad
import Garnix.NixConfig
import Garnix.Prelude
import Garnix.Sandbox
import Garnix.Types hiding (owner, repo)
import Network.URI
import System.Directory.Extra (canonicalizePath, doesPathExist)
import System.FilePath (isAbsolute)

-- | What a build needs to fetch its flake inputs.
data InputAuthorization = InputAuthorization
  { -- | Nix settings for the whole build: GitHub's @access-tokens@.
    authorizationNixConfig :: NixConfig,
    -- | Credentials that may only be used to fetch 'authorizationPrefetch'.
    -- They reach every repository the instance's bot sees, so evaluation,
    -- which can fetch anything @flake.nix@ names, never gets them.
    authorizationNetRc :: [NetRcEntry],
    -- | The private inputs to fetch into the store before evaluation, as
    -- their lock file pins them. Evaluation finds them there by their hash.
    authorizationPrefetch :: [Value]
  }

-- | A flake input on an instance's host that names no single repository
-- there, and must not be fetched.
data RejectedInput = RejectedInput
  { rejectedInputForge :: ForgeSlug,
    -- | The URL, or the host of a @github@-style input
    rejectedInputUrl :: Text,
    rejectedInputReason :: Text
  }
  deriving stock (Eq, Ord, Show)

-- | A flake input fetched from a configured instance.
data InstanceInput = InstanceInput
  { instanceInputRepo :: RepoId,
    -- | The input as the lock file pins it, if it does
    instanceInputLocked :: Maybe Value
  }
  deriving stock (Eq, Ord, Show)

-- | Throws if access should be denied.
-- Otherwise, returns the needed authorization to fetch all flake inputs.
checkAuthorization :: (HasCallStack) => FlakeDir -> RepoConfig -> CommitInfo -> M InputAuthorization
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
  let repoInfo' = commitInfo ^. repoInfo
      self = repoInfo' ^. repoId
  -- Only the instance the repository is on hands its builds credentials, so
  -- only its inputs are matched; a GitHub repository's are left as they were.
  giteaConfigs <-
    view #forges
      <&> filter (\config -> config ^. kind == GiteaForgeKind && config ^. slug == self ^. forge)
        . map _forgeInstanceConfig
        . Map.elems
  (inputs, (rejectedUrls, instanceInputs)) <-
    aesonDecode
      "output of 'nix flake metadata --json'"
      (\metadata -> (,) <$> _parseFlakeMetaData metadata <*> _parseGiteaInputs giteaConfigs metadata)
      output
  unless (null rejectedUrls)
    $ throw
    $ OtherError
    $ T.intercalate "\n"
    $ rejectedUrls
    <&> \rejected ->
      "flake input disallowed: "
        <> rejectedInputUrl rejected
        <> " is on the forge "
        <> getForgeSlug (rejectedInputForge rejected)
        <> ", but "
        <> rejectedInputReason rejected
        <> ". Inputs on a forge must point into one of its repositories, as https://host/owner/repo does."
  githubInputs <- checkInputsAllowed curDir inputs
  let giteaInputs = map instanceInputRepo instanceInputs
      -- Only inputs on the repository's own forge can be fetched with the
      -- repository's credentials, so only they are checked and handed any.
      -- Inputs on other forges get nothing beyond the server-wide nix
      -- configuration.
      forgeInputs =
        filter (\input -> input ^. forge == self ^. forge)
          $ nubOrd
          $ map (\(GithubFlakeInput owner repo) -> RepoId githubForge owner repo) githubInputs
          <> giteaInputs
      -- Outside GitHub, where the installation token only reaches the
      -- installation's repositories, the bot token reaches everything the bot
      -- sees, so a private input must also belong to the repository's owner.
      unreachable input = input ^. forge /= githubForge && not (sameOwner input)
      -- Gitea compares logins case-insensitively. GitHub repositories keep
      -- the exact comparison they always had.
      sameOwner input
        | self ^. forge == githubForge = input ^. repoUser == self ^. repoUser
        | otherwise = T.toLower (ownerOf input) == T.toLower (ownerOf self)
      ownerOf repo = getGhLogin $ getGhRepoOwner $ repo ^. repoUser
      throwUnreachable input =
        throw $ OtherError $ showRepoId input <> " is private or doesn't exist.\nIf it is private and you would like to use it, see https://garnix.io/docs/private_inputs."
  -- A repository on another forge gets no GitHub credentials of its own, but
  -- nix still holds the server's access-tokens for github.com, which may
  -- reach private repositories: its github: inputs must be public, as seen
  -- by anyone.
  when (self ^. forge /= githubForge)
    $ forM_ (nubOrd githubInputs)
    $ \input@(GithubFlakeInput owner repo) ->
      askGithubVisibility (RepoId githubForge owner repo) >>= \case
        GithubPublic -> pure ()
        GithubNotVisible ->
          throw
            $ OtherError
            $ showPretty input
            <> " is private or doesn't exist. A repository on "
            <> getForgeSlug (self ^. forge)
            <> " can only use public GitHub repositories as inputs."
        GithubUnknown reason ->
          throw
            $ OtherError
            $ "Could not tell whether "
            <> showPretty input
            <> " is public, as a repository on "
            <> getForgeSlug (self ^. forge)
            <> " needs its GitHub inputs to be: "
            <> reason
            <> ". This is usually temporary; retry the build later."
  privateInputs <- filterM (fmap (not . isRepoPublic) . getRepoPublicity (repoInfo' ^. credentials)) forgeInputs
  selfRepoPublicity <- getRepoPublicity (repoInfo' ^. credentials) self
  selfConfig <- _forgeInstanceConfig <$> forgeInstanceFor (self ^. forge)
  let credentialsForPrivateInputs =
        let (nixConfig', netRc) = _privateInputCredentials selfConfig (repoInfo' ^. ghToken)
         in InputAuthorization
              { authorizationNixConfig = nixConfig',
                authorizationNetRc = netRc,
                authorizationPrefetch =
                  nubOrd
                    [ locked
                    | InstanceInput repo (Just locked) <- instanceInputs,
                      repo `elem` privateInputs
                    ]
              }
  -- The prefetch holds the bot token, and a private input's .gitmodules or
  -- .lfsconfig can name any repository the bot sees.
  forM_ (authorizationPrefetch credentialsForPrivateInputs) $ \locked ->
    forM_ [("submodules", "its submodules"), ("lfs", "its Git LFS files")] $ \(field, what) ->
      when (locked ^? key field . _Bool == Just True)
        $ throw
        $ OtherError
        $ "flake input disallowed: "
        <> fromMaybe "a private input" (locked ^? key "url" . _String)
        <> " is private and fetches "
        <> what
        <> ", which garnix does not support for "
        <> getForgeSlug (self ^. forge)
        <> "."
  case privateInputs of
    [] -> pure $ InputAuthorization (NixConfig mempty) [] []
    _
      | isRepoPublic selfRepoPublicity -> do
          let skipPrivateInputChecks = repoConfig ^. skipPrivateInputsCheckForCollaborators
          unless skipPrivateInputChecks $ do
            throw
              $ OtherError
              $ "Public repository has private dependencies, which is not allowed. Private dependencies: "
              <> T.unwords (fmap showRepoId privateInputs)
          forM_ (filter unreachable privateInputs) throwUnreachable
          pure credentialsForPrivateInputs
      | isJust $ commitInfo ^. prFromFork ->
          throw
            $ OtherError
              "Repository has private dependencies, but PR is from fork."
    _ -> do
      baseRepoCollaborators' <- getRepoCollaborators (repoInfo' ^. credentials) self <?> "Getting repo collaborators"
      baseRepoCollaborators <- case baseRepoCollaborators' of
        RepoNotFound -> throw $ OtherError "checkAuthorization: base repo not found"
        GhCollaborators collaborators -> pure collaborators
      forM_ privateInputs $ \privateInput -> do
        unless (sameOwner privateInput) $ do
          throwUnreachable privateInput
        let skipPrivateInputChecks = repoConfig ^. skipPrivateInputsCheckForCollaborators
        unless skipPrivateInputChecks $ do
          thisInputCollaborators' <-
            getRepoCollaborators (repoInfo' ^. credentials) privateInput
          thisInputCollaborators <- case thisInputCollaborators' of
            RepoNotFound -> throw $ OtherError $ "checkAuthorization: repo " <> (getGhLogin . getGhRepoOwner $ privateInput ^. repoUser) <> "/" <> getGhRepoName (privateInput ^. repoName) <> " not found"
            GhCollaborators collaborators -> pure collaborators
          let missingUsers = filter (`notElem` thisInputCollaborators) baseRepoCollaborators
          unless (null missingUsers)
            $ throw
            $ OtherError
            $ "Aborting, as some collaborators of this repository "
            <> "don't have access to a required private dependency ("
            <> showRepoId privateInput
            <> "). The users missing permissions are: "
            <> showPretty missingUsers
      pure credentialsForPrivateInputs

-- | How a GitHub repository looks to the token nix holds for github.com.
data GithubVisibility
  = GithubPublic
  | -- | Private, or not there.
    GithubNotVisible
  | -- | GitHub gave no answer, for instance under its rate limit.
    GithubUnknown Text
  deriving stock (Eq, Show)

-- | Whether a GitHub repository is public. GitHub is asked with the token nix
-- holds for github.com, which is what could fetch a private repository, or
-- anonymously without one. Answers are kept for a while: every flake has a
-- few @github:@ inputs, and an anonymous client gets 60 requests an hour.
askGithubVisibility :: RepoId -> M GithubVisibility
askGithubVisibility repo = do
  cache <- view #githubPublicityCache
  now <- liftIO getCurrentTime
  cached <- liftIO $ Map.lookup repo <$> readTVarIO cache
  case cached of
    Just (expiry, public) | now < expiry -> pure $ if public then GithubPublic else GithubNotVisible
    _ -> do
      token <- githubAccessToken <$> view #userNixConfig
      gh <- view #githubInterface
      visibility <- _githubVisibility <$> tryEither (_githubInterfaceGetRepoPrivate gh token repo)
      let remember public =
            liftIO
              $ atomically
              $ modifyTVar' cache
              $ Map.insert repo (addUTCTime githubVisibilityLifetime now, public)
              . Map.filter ((> now) . fst)
      case visibility of
        GithubPublic -> remember True
        GithubNotVisible -> remember False
        GithubUnknown _ -> pure ()
      pure visibility

githubVisibilityLifetime :: NominalDiffTime
githubVisibilityLifetime = 10 * 60

-- | GitHub's answer to whether a repository is private. Only a 404 means the
-- token cannot see it; any other failure says nothing about the repository.
_githubVisibility :: Either (Either SomeException ErrorWithContext) Bool -> GithubVisibility
_githubVisibility = \case
  Right False -> GithubPublic
  Right True -> GithubNotVisible
  Left (Right e) -> case err e of
    NoSuchRepo {} -> GithubNotVisible
    GarnixAppUnauthorized {} -> GithubUnknown "GitHub refused the request, as it does once its rate limit is exhausted"
    GithubRequestTimeout -> GithubUnknown "GitHub did not answer in time"
    other -> GithubUnknown $ "GitHub failed: " <> show other
  Left (Left e) -> GithubUnknown $ "GitHub failed: " <> show e

-- | What nix needs to fetch the private inputs of a repository on the given
-- forge, which are all on that same forge: GitHub's token goes in
-- @access-tokens@, which nix only applies to @github:@ inputs; a Gitea
-- instance's goes in a netrc entry for its host, which nix applies to
-- @tarball@ inputs and the git it runs reads for @git+https@ ones.
_privateInputCredentials :: ForgeConfig -> GhToken -> (NixConfig, [NetRcEntry])
_privateInputCredentials config token = case config ^. kind of
  GithubForgeKind -> (githubAccessTokenNixConfig token, [])
  GiteaForgeKind -> (NixConfig mempty, toList $ giteaNetRcEntry config token)

-- | The flake inputs fetched from one of the given Gitea instances, by the
-- URL (@git@, @tarball@ and @file@ inputs) or host (@github@, @gitlab@ and
-- @sourcehut@ ones) of their original and locked references; and, first, the
-- inputs on an instance's host that 'giteaInputRepo' cannot pin to a single
-- repository, with the reason, which must not be fetched at all.
_parseGiteaInputs :: [ForgeConfig] -> Value -> Parser ([RejectedInput], [InstanceInput])
_parseGiteaInputs [] _ = pure ([], [])
_parseGiteaInputs configs json = do
  locks <- withObject "flake metadata" pure json >>= (.: "locks")
  rootKey <- locks .: "root"
  nodes :: KeyMap Value <- KeyMap.delete rootKey <$> (locks .: "nodes")
  results <- fmap concat $ forM (KeyMap.elems nodes) $ withObject "flake input node" $ \node -> do
    locked :: Maybe Value <- node .:? "locked"
    fmap concat $ forM ["original", "locked"] $ \field -> do
      ref <- node .:? fromString field
      case ref of
        Nothing -> pure []
        Just ref' -> do
          url :: Maybe Text <- ref' .:? "url"
          host :: Maybe Text <- ref' .:? "host"
          pure
            $ concat
              [ [ bimap (RejectedInput (config ^. slug) u) (fmap (`InstanceInput` locked)) (giteaInputRepo config u)
                | u <- toList url
                ]
                  <> [ Left (RejectedInput (config ^. slug) h "it is not a git, tarball or file input")
                     | h <- toList host,
                       giteaInputHost config h
                     ]
              | config <- configs
              ]
  pure (nubOrd (lefts results), nubOrd (catMaybes (rights results)))

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
