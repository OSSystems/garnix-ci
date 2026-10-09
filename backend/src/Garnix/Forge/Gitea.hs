-- | Gitea (and Forgejo, which speaks the same API) as a 'Forge'.
--
-- A repository has garnix installed when it is enabled in an installation
-- ('ForgeInstallation') that still builds. garnix calls the instance for it
-- with the OAuth token of whoever installed that installation, or with
-- the instance's bot token ('_forgeConfigApiToken') when the operator
-- configured one, and only if that token can push to the repository.
module Garnix.Forge.Gitea
  ( giteaForgeApi,
    giteaToken,
    createGiteaHook,
    deleteGiteaHook,
    giteaInstallTargets,
    giteaMayInstallOn,
    giteaReposOf,
    giteaRepoExists,
    instanceHost,
    giteaCommitStatus,
    giteaStatusState,
    giteaRemoteUrl,
    giteaNetRcEntry,
    giteaInputRepo,
    giteaInputHost,
  )
where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Array, _Bool, _Integer, _Integral, _String)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Garnix.DB.Installations qualified as DB
import Garnix.Forge.Installation
import Garnix.GithubInterface (retryWreq)
import Garnix.GithubUserToken (withUserToken)
import Garnix.Monad
import Garnix.NixConfig (NetRcEntry (..))
import Garnix.Prelude
import Garnix.Types hiding (statusCode)
import Network.URI (URI (..), URIAuth (..), escapeURIString, isUnreserved, parseURI, unEscapeString)
import Network.Wreq qualified as Wreq
import Text.Read (readMaybe)

-- | The 'Forge' for one Gitea instance.
giteaForgeApi :: ForgeConfig -> Forge
giteaForgeApi config =
  Forge
    { _forgeResolveCredentials = fmap (fmap (credentialsFor config)) . buildingInstallation config,
      _forgeResolveRepo = resolveInstalledRepo config,
      _forgeGetDefaultBranch = \credentials' repo -> do
        -- Without credentials the instance is asked anonymously, which only
        -- sees public repositories.
        token <- traverse (giteaToken config) credentials'
        response <- apiGet config "getDefaultBranch" token (repoPath repo) []
        case statusOf response of
          s | s == 404 || s == 403 || s == 401 -> pure Nothing
          _ -> do
            body <- expectOk "getDefaultBranch" repo response
            pure $ Branch <$> body ^? key "default_branch" . _String,
      _forgeGetHeadCommit = \token repo (Branch branch') -> do
        response <- apiGet config "getHeadCommit" (Just token) (repoPath repo <> ["branches"] <> T.splitOn "/" branch') []
        body <- expectOk "getHeadCommit" repo response
        case body ^? key "commit" . key "id" . _String of
          Just sha' -> pure $ CommitHash sha'
          Nothing -> throw $ OtherError $ "Could not get the HEAD commit of " <> showRepoId repo <> " on branch " <> branch',
      _forgeNewBuildReport = \repoInfo report -> do
        fromRelativeUrl <- relativeUrlConverter
        body <- postStatus repoInfo (giteaCommitStatus fromRelativeUrl report) (_ghRunReportCommit report)
        -- Gitea has no run to come back to: a status is replaced by posting
        -- another one with the same context on the same commit. The id handed
        -- back only marks the build as reported; it is negated so that it can
        -- never be mistaken for a GitHub check run id.
        case body ^? key "id" . _Integer of
          Just statusId -> pure $ GhRunId $ negate $ fromInteger statusId
          Nothing -> throw $ FailedToParseCreateReportResult $ fromMaybe Aeson.Null $ Aeson.decode body,
      _forgeUpdateBuildReport = \_runId report repoInfo ->
        -- Commit statuses carry no logs, so while a build runs there is nothing
        -- to update: the pending status posted by '_forgeNewBuildReport' stands.
        -- Posting it again on every log line would only pile up identical rows
        -- in the commit's status history.
        when (_ghRunReportStatus report /= RunReportStatusInProgress) $ do
          fromRelativeUrl <- relativeUrlConverter
          void $ postStatus repoInfo (giteaCommitStatus fromRelativeUrl report) (_ghRunReportCommit report),
      _forgeDoesRepoFileExist = \commitInfo path -> do
        let repoInfo' = commitInfo ^. repoInfo
            repo = repoInfo' ^. repoId
        token <- tokenOf repoInfo'
        response <-
          apiGet
            config
            "doesRepoFileExist"
            (Just token)
            (repoPath repo <> ["contents"] <> T.splitOn "/" (cs path))
            [("ref", getCommitHash $ commitInfo ^. commit)]
        if statusOf response == 404
          then pure FileDoesntExist
          else expectOk "doesRepoFileExist" repo response $> FileExists,
      _forgeGetRemote = \commitInfo -> giteaRemoteUrl config commitInfo,
      _forgeGetRepoCollaborators = \credentials' repo -> do
        token <- giteaToken config credentials'
        let fetchPage page = do
              response <- apiGet config "getRepoCollaborators" (Just token) (repoPath repo <> ["collaborators"]) (pageParams page)
              if statusOf response == 404
                then pure Nothing
                else Just <$> expectOkPage "getRepoCollaborators" repo response
        paginate fetchPage <&> \case
          Nothing -> RepoNotFound
          Just pages -> GhCollaborators $ pages ^.. each . _Array . each . key "login" . _String . to GhLogin,
      _forgeGetRepoPublicity = \credentials' repo -> do
        token <- giteaToken config credentials'
        body <- apiGet config "getRepoPublicity" (Just token) (repoPath repo) [] >>= expectOk "getRepoPublicity" repo
        case body ^? key "private" . _Bool of
          Just private -> pure $ RepoIsPublic $ not private
          Nothing -> throw $ OtherError $ "getRepoPublicity: no 'private' field for " <> showRepoId repo,
      _forgeOpenPullRequest = \repo pr -> do
        token <-
          resolveInstalledRepo config repo >>= \case
            Nothing -> throw $ GarnixAppUnauthorized (repo ^. repoUser) (repo ^. repoName)
            Just repoInfo' -> tokenOf repoInfo'
        response <-
          apiPost config "openPullRequest" token (repoPath repo <> ["pulls"])
            $ Aeson.object
              [ "title" .= (pr ^. title),
                "body" .= (pr ^. body),
                "head" .= getBranch (pr ^. headBranch),
                "base" .= getBranch (pr ^. baseBranch)
              ]
        expectOk "openPullRequest" repo response
          >>= maybe (throw $ OtherError "openPullRequest: no html_url in Gitea's answer") (pure . PullRequestResult)
          . (^? key "html_url" . _String),
      _forgeExchangeOauthCode = \callbackUrl (OAuthCode code) ->
        postToTokenEndpoint
          config
          "exchangeOauthCode"
          "authorization_code"
          ["code" Wreq.:= code, "redirect_uri" Wreq.:= callbackUrl],
      _forgeRefreshUserCredentials = \token ->
        postToTokenEndpoint
          config
          "refreshUserCredentials"
          "refresh_token"
          ["refresh_token" Wreq.:= token],
      _forgeGetCurrentUser = \accessToken' -> do
        response <- apiGet config "getCurrentUser" (Just $ GhToken accessToken') ["user"] []
        when (statusOf response == 401) $ throw GithubUserTokenRejected
        when (statusOf response >= 400)
          $ throw
          $ OtherError
          $ "getCurrentUser: unexpected status from Gitea: "
          <> show (statusOf response)
        let body = response ^. Wreq.responseBody
        login' <- maybe (throw $ OtherError "getCurrentUser: no login in Gitea's answer") pure $ body ^? key "login" . _String
        email' <- case body ^? key "email" . _String of
          Just e | not (T.null e) -> pure e
          _ -> throw $ OtherError "No email address"
        pure (GhLogin login', Email email', body ^? key "is_admin" . _Bool == Just True),
      _forgeGetPullRequestsForCommit = \repoInfo' (CommitHash commit') -> do
        token <- tokenOf repoInfo'
        let repo = repoInfo' ^. repoId
            fetchPage page =
              Just
                <$> ( apiGet
                        config
                        "getPullRequestsForCommit"
                        (Just token)
                        (repoPath repo <> ["pulls"])
                        (("state", "open") : pageParams page)
                        >>= expectOkPage "getPullRequestsForCommit" repo
                    )
        pages <- fromMaybe [] <$> paginate fetchPage
        pure
          $ pages
          ^.. each
          . _Array
          . each
          . filtered (\pr -> pr ^? key "head" . key "sha" . _String == Just commit')
          . key "number"
          . _Integral
          . to GhPullRequestId,
      _forgeCommentOnPullRequest = \repoInfo' (GhPullRequestId prId) body' -> do
        let repo = repoInfo' ^. repoId
        token <- tokenOf repoInfo'
        apiPost config "commentOnPullRequest" token (repoPath repo <> ["issues", show prId, "comments"]) (Aeson.object ["body" .= body'])
          >>= void
          . expectOk "commentOnPullRequest" repo
    }
  where
    postStatus repoInfo' status' (CommitHash sha') = do
      let repo = repoInfo' ^. repoId
      token <- tokenOf repoInfo'
      apiPost config "postCommitStatus" token (repoPath repo <> ["statuses", sha']) status'
        >>= expectOk "postCommitStatus" repo
    -- The token in the 'RepoInfo' may be older than an installer's token
    -- lasts: a build can run for longer than an hour.
    tokenOf repoInfo' = giteaToken config (repoInfo' ^. credentials)

-- | The commit status a build report becomes. Its context is the report's
-- name, so that every report on a commit has its own line, and a later status
-- for the same report replaces the earlier one.
giteaCommitStatus ::
  -- | Turns a garnix-relative URL into an absolute one
  (Text -> Text) ->
  GhRunReport ->
  Aeson.Value
giteaCommitStatus fromRelativeUrl report =
  Aeson.object
    $ [ "state" .= giteaStatusState (_ghRunReportStatus report),
        "context" .= _ghRunReportName report,
        -- Older Gitea versions store the description in a VARCHAR(255).
        "description" .= T.take 255 (_ghRunReportSummary report)
      ]
    <> maybe [] (\url -> ["target_url" .= fromRelativeUrl url]) (_ghRunReportUrl report)

-- | Gitea's commit status states are @pending@, @success@, @error@, @failure@
-- and @warning@. A build that never got to an outcome (it timed out, or was
-- cancelled) is an @error@, not a @failure@ of the code under test.
giteaStatusState :: RunReportStatus -> Text
giteaStatusState = \case
  RunReportStatusInProgress -> "pending"
  RunReportStatusSuccess -> "success"
  RunReportStatusFailure -> "failure"
  RunReportStatusTimeout -> "error"
  RunReportStatusCancelled -> "error"

-- | Where to clone a commit from. The repository itself is cloned with the
-- repository's token ('giteaToken'), as user @x-access-token@ so that 'Garnix.Build.Checkout.cleanRemote'
-- scrubs it from the checkout; a pull request's fork is cloned anonymously,
-- like GitHub's.
giteaRemoteUrl :: ForgeConfig -> CommitInfo -> M RemoteUrl
giteaRemoteUrl config commitInfo = do
  uri <- webUri config
  token <- case commitInfo ^. prFromFork of
    Just _ -> pure Nothing
    Nothing -> Just <$> giteaToken config (commitInfo ^. repoInfo . credentials)
  let base = withoutTrailingSlash (cs $ uriPath uri)
      origin auth' = cs (uriScheme uri) <> "//" <> auth' <> host uri <> base
  pure $ RemoteUrl $ case commitInfo ^. prFromFork of
    Just (PrFromFork fromFork) -> origin "" <> "/" <> fromFork <> ".git"
    Nothing ->
      let repo = commitInfo ^. repoInfo . repoId
       in origin (maybe "" (\token' -> "x-access-token:" <> getGhToken token' <> "@") token)
            <> "/"
            <> getGhLogin (getGhRepoOwner $ repo ^. repoUser)
            <> "/"
            <> getGhRepoName (repo ^. repoName)
            <> ".git"
  where
    host uri = maybe "" (\a -> cs (uriRegName a) <> cs (uriPort a)) (uriAuthority uri)

-- * Hosts

-- | The host name of a forge's web URL, without port: what netrc and git
-- match credentials on.
forgeHost :: ForgeConfig -> Maybe Text
forgeHost config = do
  uri <- parseURI (cs $ config ^. webUrl)
  cs . uriRegName <$> uriAuthority uri

-- | A forge's host as netrc matches it: case-insensitively, with or without a
-- trailing dot, percent-decoded. Forges with the same one cannot be told apart
-- by the credentials garnix hands nix.
instanceHost :: ForgeConfig -> Maybe Text
instanceHost config = normaliseHost <$> forgeHost config

normaliseHost :: Text -> Text
normaliseHost = T.dropWhileEnd (== '.') . T.toLower . cs . unEscapeString . cs

-- * Calling the API

-- | The installation a repository is enabled in, if it still builds. One
-- that needs a reinstall builds nothing, and says so.
buildingInstallation :: ForgeConfig -> RepoId -> M (Maybe ForgeInstallation)
buildingInstallation config repo =
  DB.enabledInstallation repo >>= \case
    Just installation | forgeInstallationForge installation == config ^. slug ->
      case forgeInstallationStatus installation of
        InstallationActive -> pure $ Just installation
        NeedsReinstall reason -> do
          log Notice
            $ "not building "
            <> showRepoId repo
            <> ": garnix on "
            <> describeTarget (forgeInstallationTarget installation)
            <> " needs to be reinstalled ("
            <> reason
            <> ")"
          pure Nothing
    _ -> pure Nothing

-- | The bot token, when the operator configured one: it takes precedence over
-- the installer's.
credentialsFor :: ForgeConfig -> ForgeInstallation -> ForgeCredentials
credentialsFor config installation
  | isJust (config ^. apiToken) = ApiTokenCredentials
  | otherwise = ForgeInstallationCredentials (forgeInstallationId installation)

-- | The repository, if garnix is installed on it, with the token garnix calls
-- the instance with, which must be able to push to it: commit statuses take
-- write access. An installer's token the instance refuses, and that cannot
-- be renewed, marks the installation as needing a reinstall; so does an
-- installer who can no longer install on the target (who left the
-- organisation, or is no longer an administrator).
resolveInstalledRepo :: (HasCallStack) => ForgeConfig -> RepoId -> M (Maybe RepoInfo)
resolveInstalledRepo config repo =
  buildingInstallation config repo >>= \case
    Nothing -> pure Nothing
    Just installation -> do
      let target = forgeInstallationTarget installation
          resolveWith credentials' holder token = do
            response <- apiGet config "resolveRepo" (Just token) (repoPath repo) []
            when (statusOf response == 401 && holder == HeldByInstaller) $ throw GithubUserTokenRejected
            access <-
              either (throw . unexpectedStatus) pure
                $ repoAccess (statusOf response) (Aeson.decode $ response ^. Wreq.responseBody)
            forM_ (accessProblem (describeTarget target) repo access) $ log Notice . ("resolveRepo: " <>)
            case (access, holder) of
              (CanPush, _) -> pure $ Resolved (RepoInfo credentials' token repo)
              (_, HeldByBot) -> pure Unusable
              -- A repository the installer cannot use is theirs to fix if
              -- they may still install there (it was deleted, say); if they
              -- may not, the installation has lost its installer.
              (_, HeldByInstaller) -> (\eligible -> if eligible then Unusable else InstallerIneligible) <$> giteaMayInstallOn config token target
          settle = \case
            Resolved repoInfo' -> pure $ Just repoInfo'
            Unusable -> pure Nothing
            InstallerIneligible -> do
              forM_ (forgeInstallationInstaller installation) $ \installer ->
                stopInstallation config installation installer
                  $ getGhLogin installer
                  <> ", who installed it, may no longer install garnix on "
                  <> describeTarget target
              pure Nothing
      case credentialsFor config installation of
        credentials'@ApiTokenCredentials -> settle =<< resolveWith credentials' HeldByBot =<< botToken config
        credentials'@(ForgeInstallationCredentials installation') ->
          maybe (pure Nothing) settle =<< withInstallerToken config installation' (resolveWith credentials' HeldByInstaller)
        GithubInstallationCredentials _ -> throw $ OtherError "A Gitea forge was handed GitHub credentials"

-- | Whose token resolves a repository.
data TokenHolder = HeldByInstaller | HeldByBot
  deriving stock (Eq, Show)

-- | What a token may do with a repository.
data RepoAccess = CannotSee | CannotPush | CanPush
  deriving stock (Eq, Show)

-- | The access Gitea's answer to @GET /repos/:owner/:name@ (its status and
-- body) gives; 'Left' the status of an answer that is an error.
repoAccess :: Int -> Maybe Aeson.Value -> Either Int RepoAccess
repoAccess status' body
  | status' `elem` [401, 403, 404] = Right CannotSee
  | status' >= 200 && status' < 300 =
      Right $ if body ^? _Just . key "permissions" . key "push" . _Bool == Just True then CanPush else CannotPush
  | otherwise = Left status'

unexpectedStatus :: Int -> Error
unexpectedStatus status' = OtherError $ "resolveRepo: Gitea answered with the unexpected status " <> show status'

-- | What is wrong with the access the token of garnix on a target (as
-- 'describeTarget' puts it) has to a repository, if anything.
accessProblem :: Text -> RepoId -> RepoAccess -> Maybe Text
accessProblem target repo = \case
  CannotSee -> Just $ "the token of garnix on " <> target <> " cannot see " <> showRepoId repo
  CannotPush -> Just $ "the token of garnix on " <> target <> " cannot push to " <> showRepoId repo <> ", so it cannot report builds on it"
  CanPush -> Nothing

-- | Where resolving a repository through an installation ends.
data Resolution
  = Resolved RepoInfo
  | -- | Not with this token, for a reason the installation does not change.
    Unusable
  | -- | The installer may no longer install on the target.
    InstallerIneligible

-- | The token to call the instance with, for a repository's credentials.
giteaToken :: (HasCallStack) => ForgeConfig -> ForgeCredentials -> M GhToken
giteaToken config = \case
  ApiTokenCredentials -> botToken config
  ForgeInstallationCredentials installation ->
    withInstallerToken config installation pure
      >>= maybe (throw $ OtherError $ "garnix needs to be reinstalled on " <> getForgeSlug (config ^. slug)) pure
  GithubInstallationCredentials _ -> throw $ OtherError "A Gitea forge was handed GitHub credentials"

-- | Runs the action with the installer's token, renewing it when it runs
-- out or when the action throws 'GithubUserTokenRejected'. 'Nothing', and
-- the installation marked as needing a reinstall, when the installer is gone
-- or their token cannot be renewed: it was revoked, or they lost access.
withInstallerToken :: (HasCallStack) => ForgeConfig -> ForgeInstallationId -> (GhToken -> M a) -> M (Maybe a)
withInstallerToken config installation action =
  DB.getForgeInstallation installation >>= \case
    Nothing -> pure Nothing
    Just installation' -> case (forgeInstallationStatus installation', forgeInstallationInstaller installation') of
      (InstallationActive, Just installer) -> do
        result <- try $ withUserToken (ForgeLogin (config ^. slug) installer) action
        case result of
          Right a -> pure $ Just a
          Left e | ForgeSessionExpired _ <- err e -> do
            stopInstallation config installation' installer
              $ "the instance no longer accepts the token of "
              <> getGhLogin installer
              <> ", who installed it"
            pure Nothing
          Left e -> throwError e
      (NeedsReinstall _, _) -> pure Nothing
      (InstallationActive, Nothing) -> do
        log Error
          $ "garnix on "
          <> describeTarget (forgeInstallationTarget installation')
          <> " of "
          <> getForgeSlug (config ^. slug)
          <> " is active without an installer, which the database forbids; it builds nothing"
        pure Nothing

-- | Marks the installation as needing a reinstall, if the installer it
-- failed with is still its installer.
stopInstallation :: ForgeConfig -> ForgeInstallation -> GhLogin -> Text -> M ()
stopInstallation config installation installer reason = do
  DB.markNeedsReinstall (forgeInstallationId installation) installer reason
  log Error
    $ "garnix on "
    <> describeTarget (forgeInstallationTarget installation)
    <> " of "
    <> getForgeSlug (config ^. slug)
    <> " needs to be reinstalled: "
    <> reason
    <> ". It builds nothing until someone allowed to install it there reinstalls it."

botToken :: (HasCallStack) => ForgeConfig -> M GhToken
botToken config = case config ^. apiToken of
  Just token -> pure token
  Nothing -> throw $ OtherError $ "The Gitea forge " <> getForgeSlug (config ^. slug) <> " has no API token configured"

repoPath :: RepoId -> [Text]
repoPath repo = ["repos", getGhLogin (getGhRepoOwner $ repo ^. repoUser), getGhRepoName (repo ^. repoName)]

apiUrlFor :: ForgeConfig -> [Text] -> String
apiUrlFor config segments =
  cs
    $ withoutTrailingSlash (config ^. apiUrl)
    <> "/"
    <> T.intercalate "/" (map (cs . escapeURIString isUnreserved . cs) segments)

withoutTrailingSlash :: Text -> Text
withoutTrailingSlash = T.dropWhileEnd (== '/')

baseOptions :: Wreq.Options -> Maybe GhToken -> Wreq.Options
baseOptions options token =
  options
    & Wreq.header "Accept"
    .~ ["application/json"]
    & Wreq.checkResponse
    ?~ (\_ _ -> pure ())
    & maybe identity (\(GhToken t) -> Wreq.header "Authorization" .~ ["token " <> cs t]) token

apiGet :: ForgeConfig -> Text -> Maybe GhToken -> [Text] -> [(Text, Text)] -> M (Wreq.Response LazyByteString)
apiGet config method token segments params = do
  let url = apiUrlFor config segments
  withTextSpan ("gitea-api", method) $ retryWreq $ withWreqOptions $ \options ->
    Wreq.getWith
      (foldl' (\o (k, v) -> o & Wreq.param k .~ [v]) (baseOptions options token) params)
      url

apiPost :: ForgeConfig -> Text -> GhToken -> [Text] -> Aeson.Value -> M (Wreq.Response LazyByteString)
apiPost config method token segments payload = do
  let url = apiUrlFor config segments
  withTextSpan ("gitea-api", method) $ retryWreq $ withWreqOptions $ \options ->
    Wreq.postWith (baseOptions options (Just token)) url payload

apiDelete :: ForgeConfig -> Text -> GhToken -> [Text] -> M (Wreq.Response LazyByteString)
apiDelete config method token segments = do
  let url = apiUrlFor config segments
  withTextSpan ("gitea-api", method) $ retryWreq $ withWreqOptions $ \options ->
    Wreq.deleteWith (baseOptions options (Just token)) url

statusOf :: Wreq.Response a -> Int
statusOf response = response ^. Wreq.responseStatus . Wreq.statusCode

-- | The body of a successful answer. A refusal (401, 403, 404) becomes the
-- error the caller makes of it; anything else from 400 up an 'OtherError'.
-- The subject, if any, is what the call was about, for the logs.
expectOkAbout :: (HasCallStack) => Text -> Text -> (Int -> Error) -> Wreq.Response LazyByteString -> M LazyByteString
expectOkAbout method subject refusal response = case statusOf response of
  s | s `elem` [401, 403, 404] -> do
    log Informational $ method <> ": Gitea answered " <> show s <> about
    throw $ refusal s
  s | s >= 400 -> do
    log Error $ method <> ": Gitea answered " <> show s <> about <> ": " <> cs (response ^. Wreq.responseBody)
    throw $ OtherError $ "Unexpected Gitea response for " <> method <> ": " <> show s
  _ -> pure $ response ^. Wreq.responseBody
  where
    about = if T.null subject then "" else " for " <> subject

-- | 'expectOkAbout' a repository: errors become the same 'Error's the
-- GitHub forge raises, so callers need not care which forge answered.
expectOk :: (HasCallStack) => Text -> RepoId -> Wreq.Response LazyByteString -> M LazyByteString
expectOk method repo = expectOkAbout method (showRepoId repo) $ \case
  404 -> NoSuchRepo {_owner = repo ^. repoUser, _name = repo ^. repoName}
  _ -> GarnixAppUnauthorized (repo ^. repoUser) (repo ^. repoName)

-- | 'expectOkAbout' no repository: a refusal is the caller's.
expectOkFor :: (HasCallStack) => Text -> Wreq.Response LazyByteString -> M LazyByteString
expectOkFor method = expectOkAbout method "" $ \s -> ForbiddenWithMessage $ "the forge refused " <> method <> " (" <> show s <> ")"

expectOkPageFor :: (HasCallStack) => Text -> Wreq.Response LazyByteString -> M Page
expectOkPageFor method response = pageOf response <$> expectOkFor method response

pageParams :: Int -> [(Text, Text)]
pageParams page = [("page", show page), ("limit", "50")]

-- | One page of a list: its body, and the length of the whole list when
-- Gitea says (@X-Total-Count@).
data Page = Page LazyByteString (Maybe Int)

expectOkPage :: (HasCallStack) => Text -> RepoId -> Wreq.Response LazyByteString -> M Page
expectOkPage method repo response = pageOf response <$> expectOk method repo response

pageOf :: Wreq.Response a -> LazyByteString -> Page
pageOf response body' = Page body' $ response ^? Wreq.responseHeader "X-Total-Count" >>= readMaybe . cs

-- | Fetches pages (numbered from 1) until the list is complete: until a page
-- comes back empty, or the pages so far hold as many items as Gitea counts.
-- A short page does not end the list, since Gitea caps the page size at its
-- @MAX_RESPONSE_ITEMS@, which may be below the size asked for. A page equal
-- to the one before also ends it, so that a server ignoring @page@ cannot
-- keep garnix asking forever. 'Nothing' if the first page is 'Nothing'; a
-- later page that is 'Nothing' is an error, not the end of the list, since
-- reading on would act on part of it.
paginate :: (HasCallStack) => (Int -> M (Maybe Page)) -> M (Maybe [LazyByteString])
paginate fetchPage = go 1 0 Nothing
  where
    go :: Int -> Int -> Maybe LazyByteString -> M (Maybe [LazyByteString])
    go page seen previous =
      fetchPage page >>= \case
        Nothing
          | page == 1 -> pure Nothing
          | otherwise -> throw $ OtherError $ "Gitea answered 404 for page " <> show page <> " of a list whose first page it served"
        Just (Page body' total)
          | Just body' == previous -> pure $ Just []
          | otherwise -> do
              let items = length (body' ^.. _Array . each)
                  seen' = seen + items
              if items == 0 || maybe False (seen' >=) total
                then pure $ Just [body']
                else do
                  rest <- go (page + 1) seen' (Just body')
                  pure $ Just $ body' : fromMaybe [] rest

webUri :: ForgeConfig -> M URI
webUri config = case parseURI (cs $ config ^. webUrl) of
  Just uri -> pure uri
  Nothing -> throw $ OtherError $ "The web URL of forge " <> getForgeSlug (config ^. slug) <> " is not a URL"

-- * Installing

-- | Creates the webhook of an installation on its target, with the token of
-- whoever installs it: a user webhook, an organisation webhook, or a system
-- webhook, which reaches every repository on the instance (a "default"
-- webhook would only be copied into repositories created later). Answers its
-- id.
createGiteaHook :: (HasCallStack) => ForgeConfig -> GhToken -> InstallTarget -> Text -> Text -> M Integer
createGiteaHook config token target url secret = do
  let hookConfig =
        ["url" .= url, "content_type" .= ("json" :: Text), "secret" .= secret]
          <> ["is_system_webhook" .= ("true" :: Text) | target == InstallOnInstance]
  response <-
    apiPost config "createHook" token (hooksPath target)
      $ Aeson.object
        [ "type" .= ("gitea" :: Text),
          "config" .= Aeson.object hookConfig,
          "events" .= (["push", "pull_request", "pull_request_sync"] :: [Text]),
          "branch_filter" .= ("*" :: Text),
          "active" .= True
        ]
  body' <- expectOkFor "createHook" response
  maybe (throw $ OtherError "createHook: no id in Gitea's answer") pure $ body' ^? key "id" . _Integer

-- | Deletes an installation's webhook, as far as the token can: whether it is
-- gone (or already was).
deleteGiteaHook :: (HasCallStack) => ForgeConfig -> GhToken -> InstallTarget -> Integer -> M Bool
deleteGiteaHook config token target hookId = do
  response <- apiDelete config "deleteHook" token (hooksPath target <> [show hookId])
  let status' = statusOf response
      gone = status' < 300 || status' == 404
  unless gone
    $ log Notice
    $ "deleteHook: Gitea answered "
    <> show status'
    <> " deleting the webhook "
    <> show hookId
    <> " of "
    <> describeTarget target
  pure gone

hooksPath :: InstallTarget -> [Text]
hooksPath = \case
  InstallOnUser _ -> ["user", "hooks"]
  InstallOnOrg org -> ["orgs", getGhLogin org, "hooks"]
  InstallOnInstance -> ["admin", "hooks"]

-- | Who the token belongs to, and whether the instance calls them a site
-- administrator.
currentUser :: (HasCallStack) => ForgeConfig -> GhToken -> M (GhLogin, Bool)
currentUser config token = do
  response <- apiGet config "currentUser" (Just token) ["user"] []
  when (statusOf response == 401) $ throw GithubUserTokenRejected
  body' <- expectOkFor "currentUser" response
  login' <- maybe (throw $ OtherError "currentUser: no login in Gitea's answer") pure $ body' ^? key "login" . _String
  pure (GhLogin login', body' ^? key "is_admin" . _Bool == Just True)

-- | Whether the token's user owns the organisation: only owners may create
-- its webhooks.
ownsOrg :: (HasCallStack) => ForgeConfig -> GhToken -> GhLogin -> GhLogin -> M Bool
ownsOrg config token user org = do
  response <- apiGet config "orgPermissions" (Just token) ["users", getGhLogin user, "orgs", getGhLogin org, "permissions"] []
  case statusOf response of
    s | s == 404 || s == 403 -> pure False
    _ -> do
      body' <- expectOkFor "orgPermissions" response
      pure $ body' ^? key "is_owner" . _Bool == Just True

-- | Everything the token's user may install garnix on: their own account,
-- the organisations they own, and the instance if they administer it. The
-- instance is asked, never what it said at the user's last login.
giteaInstallTargets :: (HasCallStack) => ForgeConfig -> GhToken -> M [InstallTarget]
giteaInstallTargets config token = do
  (user, isAdmin) <- currentUser config token
  let fetchPage page =
        Just <$> (apiGet config "listOrgs" (Just token) ["user", "orgs"] (pageParams page) >>= expectOkPageFor "listOrgs")
  orgs <- maybe [] (concatMap orgLogins) <$> paginate fetchPage
  owned <- filterM (ownsOrg config token user) (nubOrd orgs)
  pure $ [InstallOnUser user] <> map InstallOnOrg owned <> [InstallOnInstance | isAdmin]
  where
    -- Gitea names an organisation's login @name@, and also @username@.
    orgLogins page = page ^.. _Array . each . to (\org -> org ^? key "name" . _String <|> org ^? key "username" . _String) . _Just . to GhLogin

-- | Whether the token's user may install garnix on the target, and so manage
-- an installation there, as the instance says now.
giteaMayInstallOn :: (HasCallStack) => ForgeConfig -> GhToken -> InstallTarget -> M Bool
giteaMayInstallOn config token target = do
  (user, isAdmin) <- currentUser config token
  case target of
    InstallOnUser login' -> pure $ T.toLower (getGhLogin login') == T.toLower (getGhLogin user)
    InstallOnOrg org -> ownsOrg config token user org
    InstallOnInstance -> pure isAdmin

-- | One page (from 1, at most @limit@ long) of the repositories on the
-- target, and whether each is private, with how many there are in all
-- (Gitea's @X-Total-Count@). Paging is the caller's: an instance can hold
-- thousands. Each list Gitea answers holds only the target's repositories,
-- so no page comes back short of what the target has.
giteaReposOf :: (HasCallStack) => ForgeConfig -> GhToken -> InstallTarget -> Int -> Int -> M ([(RepoId, Bool)], Int)
giteaReposOf config token target page limit = do
  Page body' total <- case target of
    -- Not @/user/repos@: that lists the organisations' repositories, and
    -- the collaborators', too.
    InstallOnUser user -> list ["users", getGhLogin user, "repos"]
    InstallOnOrg org -> list ["orgs", getGhLogin org, "repos"]
    InstallOnInstance -> list ["repos", "search"]
  let items = case target of
        -- Search answers @{"ok": true, "data": [...]}@, with every
        -- repository for a site administrator.
        InstallOnInstance -> body' ^.. key "data" . _Array . each
        _ -> body' ^.. _Array . each
  total' <- maybe (throw $ OtherError "listRepos: Gitea did not say how many repositories there are (X-Total-Count)") pure total
  pure (nubOrdOn fst $ catMaybes (map repoOf items), total')
  where
    list segments = apiGet config "listRepos" (Just token) segments [("page", show page), ("limit", show limit)] >>= expectOkPageFor "listRepos"
    repoOf repo = do
      owner <- repo ^? key "owner" . key "login" . _String
      name' <- repo ^? key "name" . _String
      pure (RepoId (config ^. slug) (GhRepoOwner $ GhLogin owner) (GhRepoName name'), repo ^? key "private" . _Bool == Just True)
    nubOrdOn f = Map.elems . Map.fromList . map (\x -> (showRepoId (f x), x)) . reverse

-- | Whether the token sees the repository.
giteaRepoExists :: (HasCallStack) => ForgeConfig -> GhToken -> RepoId -> M Bool
giteaRepoExists config token repo = do
  response <- apiGet config "repoExists" (Just token) (repoPath repo) []
  case statusOf response of
    s | s == 404 || s == 403 -> pure False
    _ -> expectOk "repoExists" repo response $> True

-- * OAuth

-- | Asks the instance's token endpoint for a user's tokens with the given
-- grant, authenticated as garnix's OAuth application.
postToTokenEndpoint :: ForgeConfig -> Text -> Text -> [Wreq.FormParam] -> M (GhUserCredentials Text)
postToTokenEndpoint config method grantType grant = do
  let endpoint = cs $ withoutTrailingSlash (config ^. webUrl) <> "/login/oauth/access_token"
      params =
        [ "client_id" Wreq.:= (config ^. oAuthClientId),
          "client_secret" Wreq.:= (config ^. oAuthClientSecret),
          "grant_type" Wreq.:= grantType
        ]
          <> grant
  response <-
    withTextSpan ("gitea-api", method) $ retryWreq $ withWreqOptions $ \options ->
      Wreq.postWith (baseOptions options Nothing) endpoint params
  now <- liftIO getCurrentTime
  let status' = statusOf response
      body' = response ^. Wreq.responseBody
      expiresAtIn k = body' ^? key k . _Integer . to (\s -> addUTCTime (fromInteger s) now)
  -- Down or overloaded (still, after 'retryWreq'): that refuses nothing, so
  -- the credentials stay, unlike after a refusal ('GithubDidntGiveUsAToken').
  when (status' >= 500 || status' == 429) $ do
    log Warning $ method <> ": Gitea answered with status " <> show status' <> ": " <> cs body'
    throw $ OtherError $ getForgeSlug (config ^. slug) <> " could not hand out a token now (status " <> show status' <> ")"
  when (status' >= 400) $ do
    log Notice $ method <> ": Gitea answered with status " <> show status' <> ": " <> cs body'
    throw GithubDidntGiveUsAToken
  case body' ^? key "access_token" . _String of
    Nothing -> do
      log Error $ method <> ": no access_token in Gitea's answer"
      throw GithubDidntGiveUsAToken
    Just accessToken' ->
      pure
        $ GhUserCredentials
          { _ghUserCredentialsAccessToken = accessToken',
            _ghUserCredentialsAccessTokenExpiresAt = expiresAtIn "expires_in",
            _ghUserCredentialsRefreshToken = body' ^? key "refresh_token" . _String,
            _ghUserCredentialsRefreshTokenExpiresAt = Nothing
          }

-- * Flake inputs

-- | The netrc entry that lets nix (and the git it runs) fetch from an instance
-- with the given token. Gitea ignores the login when the password is a token.
giteaNetRcEntry :: ForgeConfig -> GhToken -> Maybe NetRcEntry
giteaNetRcEntry config token =
  forgeHost config <&> \host ->
    NetRcEntry
      { _netRcEntryMachine = host,
        _netRcEntryLogin = "x-access-token",
        _netRcEntryPassword = getGhToken token
      }

-- | The repository on the instance a flake input's URL is fetched from.
--
-- The instance's credentials reach nix as a netrc entry, which names the host
-- alone: curl (for @tarball@ and @file@ inputs) and git (for @git@ ones) send
-- them to that host whatever the port, the user or the path in the URL. So
-- every URL on the host must name, unambiguously, the repository that is
-- fetched, or it could fetch with the bot's credentials a repository nobody
-- checked:
--
-- * @'Right' 'Nothing'@: the URL is not on the instance's host, or is fetched
--   over ssh, which never reads netrc.
-- * @'Right' ('Just' repo)@: the URL is on the instance and points into @repo@
--   (@/owner/repo@, @/owner/repo.git@, @/owner/repo/archive/…@ and the same
--   under @/api/v1/repos@, below the web URL's path).
-- * @'Left' reason@: the URL is on the instance's host but names no single
--   repository: it is outside the instance's path, has dot segments that
--   curl and git would resolve, or is not a URL at all.
giteaInputRepo :: ForgeConfig -> Text -> Either Text (Maybe RepoId)
giteaInputRepo config url = case parseURI (cs url) of
  Nothing
    | any (`T.isInfixOf` T.toLower url) (instanceHost config) -> Left "it is not a URL garnix can read"
    | otherwise -> Right Nothing
  Just target
    | not (giteaInputHost config (maybe "" (cs . uriRegName) (uriAuthority target))) -> Right Nothing
    | uriScheme target `elem` ["ssh:", "git+ssh:"] -> Right Nothing
    | uriScheme target `notElem` ["http:", "https:"] -> Left "it is not fetched over http(s)"
    | otherwise -> do
        base <- maybe (Left "the forge's web URL is not a URL") Right (parseURI (cs $ config ^. webUrl))
        baseSegments <- pathSegments base
        targetSegments <- pathSegments target
        rest <- maybe (Left "it is outside the forge's web URL") Right (stripPrefix baseSegments targetSegments)
        case fromMaybe rest (stripPrefix ["api", "v1", "repos"] rest) of
          owner : repo : _
            | validName owner && validName (dropGit repo) ->
                Right $ Just $ RepoId (config ^. slug) (GhRepoOwner $ GhLogin owner) (GhRepoName $ dropGit repo)
          _ -> Left "it does not name a repository"
  where
    -- The decoded segments of an absolute path. Dot segments, empty ones and
    -- encoded slashes would be resolved differently by curl, git and Gitea.
    pathSegments uri = case T.splitOn "/" (cs $ uriPath uri) of
      [""] -> Right []
      "" : segments -> traverse decodeSegment (dropTrailingEmpty segments)
      _ -> Left "its path is not absolute"
    dropTrailingEmpty segments = case reverse segments of
      "" : rest -> reverse rest
      _ -> segments
    decodeSegment segment =
      let decoded = cs (unEscapeString (cs segment)) :: Text
       in if decoded `elem` ["", ".", ".."] || T.any (`elem` ['/', '\\']) decoded
            then Left "its path has empty or dot segments"
            else Right decoded
    -- Gitea's owner and repository names.
    validName name =
      not (T.null name)
        && name
        `notElem` [".", ".."]
        && T.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ['-', '_', '.']) name
    dropGit repo = fromMaybe repo (T.stripSuffix ".git" repo)

-- | Whether a host name is the instance's host, as netrc would match it
-- (case-insensitively, with or without a trailing dot, percent-decoded).
giteaInputHost :: ForgeConfig -> Text -> Bool
giteaInputHost config host = Just (normaliseHost host) == instanceHost config
