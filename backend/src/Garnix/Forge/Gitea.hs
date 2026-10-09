-- | Gitea (and Forgejo, which speaks the same API) as a 'Forge'.
--
-- Unlike GitHub, garnix holds no per-repository installation here: every call
-- is made with the instance's bot token ('_forgeConfigApiToken'), and a
-- repository counts as having garnix installed when that bot can push to it.
module Garnix.Forge.Gitea
  ( giteaForgeApi,
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
import Data.Functor ((<&>))
import Data.Text qualified as T
import Garnix.GithubInterface (retryWreq)
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
    { _forgeResolveCredentials = fmap (fmap (const ApiTokenCredentials)) . resolveBotToken config,
      _forgeResolveRepo = \repo ->
        fmap (\token -> RepoInfo ApiTokenCredentials token repo) <$> resolveBotToken config repo,
      _forgeGetDefaultBranch = \credentials' repo -> do
        -- Without credentials the instance is asked anonymously, which only
        -- sees public repositories.
        token <- traverse (const $ botToken config) credentials'
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
        response <-
          apiGet
            config
            "doesRepoFileExist"
            (Just $ repoInfo' ^. ghToken)
            (repoPath repo <> ["contents"] <> T.splitOn "/" (cs path))
            [("ref", getCommitHash $ commitInfo ^. commit)]
        if statusOf response == 404
          then pure FileDoesntExist
          else expectOk "doesRepoFileExist" repo response $> FileExists,
      _forgeGetRemote = \commitInfo -> giteaRemoteUrl config commitInfo,
      _forgeGetRepoCollaborators = \_credentials repo -> do
        token <- botToken config
        let fetchPage page = do
              response <- apiGet config "getRepoCollaborators" (Just token) (repoPath repo <> ["collaborators"]) (pageParams page)
              if statusOf response == 404
                then pure Nothing
                else Just <$> expectOkPage "getRepoCollaborators" repo response
        paginate fetchPage <&> \case
          Nothing -> RepoNotFound
          Just pages -> GhCollaborators $ pages ^.. each . _Array . each . key "login" . _String . to GhLogin,
      _forgeGetRepoPublicity = \_credentials repo -> do
        token <- botToken config
        body <- apiGet config "getRepoPublicity" (Just token) (repoPath repo) [] >>= expectOk "getRepoPublicity" repo
        case body ^? key "private" . _Bool of
          Just private -> pure $ RepoIsPublic $ not private
          Nothing -> throw $ OtherError $ "getRepoPublicity: no 'private' field for " <> showRepoId repo,
      _forgeOpenPullRequest = \repo pr -> do
        token <-
          resolveBotToken config repo >>= \case
            Nothing -> throw $ GarnixAppUnauthorized (repo ^. repoUser) (repo ^. repoName)
            Just token -> pure token
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
        let repo = repoInfo' ^. repoId
            fetchPage page =
              Just
                <$> ( apiGet
                        config
                        "getPullRequestsForCommit"
                        (Just $ repoInfo' ^. ghToken)
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
        apiPost config "commentOnPullRequest" (repoInfo' ^. ghToken) (repoPath repo <> ["issues", show prId, "comments"]) (Aeson.object ["body" .= body'])
          >>= void
          . expectOk "commentOnPullRequest" repo
    }
  where
    postStatus repoInfo' status' (CommitHash sha') = do
      let repo = repoInfo' ^. repoId
      apiPost config "postCommitStatus" (repoInfo' ^. ghToken) (repoPath repo <> ["statuses", sha']) status'
        >>= expectOk "postCommitStatus" repo

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

-- | Where to clone a commit from. The repository itself is cloned with the bot
-- token, as user @x-access-token@ so that 'Garnix.Build.Checkout.cleanRemote'
-- scrubs it from the checkout; a pull request's fork is cloned anonymously,
-- like GitHub's.
giteaRemoteUrl :: ForgeConfig -> CommitInfo -> M RemoteUrl
giteaRemoteUrl config commitInfo = do
  uri <- webUri config
  let base = withoutTrailingSlash (cs $ uriPath uri)
      origin auth' = cs (uriScheme uri) <> "//" <> auth' <> host uri <> base
  pure $ RemoteUrl $ case commitInfo ^. prFromFork of
    Just (PrFromFork fromFork) -> origin "" <> "/" <> fromFork <> ".git"
    Nothing ->
      let repo = commitInfo ^. repoInfo . repoId
       in origin ("x-access-token:" <> getGhToken (commitInfo ^. repoInfo . ghToken) <> "@")
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

-- | The bot token, if the bot can push to the repository: that is what having
-- garnix installed on a Gitea repository means. Merely seeing it is not
-- enough, since the bot sees every public repository on the instance.
resolveBotToken :: (HasCallStack) => ForgeConfig -> RepoId -> M (Maybe GhToken)
resolveBotToken config repo = do
  token <- botToken config
  response <- apiGet config "resolveRepo" (Just token) (repoPath repo) []
  case statusOf response of
    s | s == 404 || s == 403 -> pure Nothing
    _ -> do
      body <- expectOk "resolveRepo" repo response
      if body ^? key "permissions" . key "push" . _Bool == Just True
        then pure $ Just token
        else do
          log Informational $ "resolveRepo: the garnix bot cannot push to " <> showRepoId repo <> ", treating it as not installed"
          pure Nothing

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

statusOf :: Wreq.Response a -> Int
statusOf response = response ^. Wreq.responseStatus . Wreq.statusCode

-- | The body of a successful answer; errors become the same 'Error's the
-- GitHub forge raises, so callers need not care which forge answered.
expectOk :: (HasCallStack) => Text -> RepoId -> Wreq.Response LazyByteString -> M LazyByteString
expectOk method repo response = case statusOf response of
  404 -> do
    log Informational $ method <> ": Gitea answered 404 for " <> showRepoId repo
    throw $ NoSuchRepo {_owner = repo ^. repoUser, _name = repo ^. repoName}
  s | s == 401 || s == 403 -> do
    log Informational $ method <> ": Gitea answered " <> show s <> " for " <> showRepoId repo
    throw $ GarnixAppUnauthorized (repo ^. repoUser) (repo ^. repoName)
  s | s >= 400 -> do
    log Error $ method <> ": Gitea answered " <> show s <> " for " <> showRepoId repo <> ": " <> cs (response ^. Wreq.responseBody)
    throw $ OtherError $ "Unexpected Gitea response for " <> method <> ": " <> show s
  _ -> pure $ response ^. Wreq.responseBody

pageParams :: Int -> [(Text, Text)]
pageParams page = [("page", show page), ("limit", "50")]

-- | One page of a list: its body, and the length of the whole list when
-- Gitea says (@X-Total-Count@).
data Page = Page LazyByteString (Maybe Int)

expectOkPage :: (HasCallStack) => Text -> RepoId -> Wreq.Response LazyByteString -> M Page
expectOkPage method repo response = do
  body' <- expectOk method repo response
  pure $ Page body' $ response ^? Wreq.responseHeader "X-Total-Count" >>= readMaybe . cs

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
