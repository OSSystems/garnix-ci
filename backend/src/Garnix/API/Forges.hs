-- | Finding the forge someone logs in through by its URL, registering a
-- Gitea/Forgejo instance garnix does not know yet, and managing registered
-- ones.
--
-- Registration is off unless the operator turns it on
-- (@services.garnixServer.allowForgeRegistration@); then only configured
-- forges are answered, and the routes that change registrations are 404s.
module Garnix.API.Forges
  ( AuthStartRequest (..),
    AuthStartAnswer (..),
    RegisterForgeRequest (..),
    ReplaceForgeSecretRequest (..),
    authStartAPI,
    registerForgeAPI,
    replaceForgeSecretAPI,
    removeForgeAPI,
    ForgeSummary (..),
    SummaryStatus (..),
    forgesAPI,
    forgeSummaries,
    disabledCandidates,
    clientAddress,
    Management (..),
    managementRefusal,
    StartDecision (..),
    startDecision,
  )
where

import Control.Exception qualified
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _String)
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word16)
import Garnix.API.Auth (authorizeLink, getOAuthNonce, newOAuthNonce, oauthCallbackPath, oauthStateCookie)
import Garnix.Access (sessionUserOf, webSessionUser)
import Garnix.DB qualified as DB (getUserById)
import Garnix.DB.Forges qualified as DB
import Garnix.Forge.OutboundGuard (ForbiddenAddress (..), GuardLimitExceeded (..), PlainHttpRefused (..), isPublicAddress)
import Garnix.Forge.Registered
import Garnix.Forge.Registry (configuredOnHost, hashRegistrationToken, registrationCookieName, registrationTokenFromCookies)
import Garnix.GithubUserToken (encryptSecret)
import Garnix.Monad
import Garnix.Prelude
import Garnix.RateLimit (RateLimiter, allowRequest)
import Garnix.Types hiding (login)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Manager)
import Network.Socket (SockAddr (..), hostAddress6ToTuple, hostAddressToTuple)
import Network.URI (URIAuth (..), parseAbsoluteURI, uriAuthority)
import Network.Wreq qualified as Wreq
import Numeric (readHex, showHex)
import Servant (noHeader)
import Servant.Auth.Server (AuthResult (Authenticated), CookieSettings (..), IsSecure (Secure))
import Text.Read (readMaybe)
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteLax)

newtype AuthStartRequest = AuthStartRequest {authStartUrl :: Text}

instance FromJSON AuthStartRequest where
  parseJSON = Aeson.withObject "AuthStartRequest" $ \o -> AuthStartRequest <$> o .: "url"

-- | Where to go with a forge URL.
data AuthStartAnswer
  = -- | The forge is known: log in through its OAuth app at this URL.
    StartLogin Text
  | -- | It is not (or its registration is unfinished): register an OAuth app
    -- on it calling back to the given URL, then submit it under this slug.
    StartRegister ForgeSlug Text
  deriving stock (Eq, Show)

instance ToJSON AuthStartAnswer where
  toJSON = \case
    StartLogin url -> Aeson.object ["login" .= url]
    StartRegister slug' callback ->
      Aeson.object ["register" .= Aeson.object ["slug" .= slug', "callback" .= callback]]

data RegisterForgeRequest = RegisterForgeRequest
  { registerUrl :: Text,
    registerClientId :: Text,
    registerClientSecret :: Text
  }

instance FromJSON RegisterForgeRequest where
  parseJSON = Aeson.withObject "RegisterForgeRequest" $ \o ->
    RegisterForgeRequest <$> o .: "url" <*> o .: "clientId" <*> o .: "clientSecret"

newtype ReplaceForgeSecretRequest = ReplaceForgeSecretRequest {replaceClientSecret :: Text}

instance FromJSON ReplaceForgeSecretRequest where
  parseJSON = Aeson.withObject "ReplaceForgeSecretRequest" $ \o -> ReplaceForgeSecretRequest <$> o .: "clientSecret"

-- | A forge instance as the frontend sees it: enough to build URLs and links,
-- none of its secrets.
data ForgeSummary = ForgeSummary
  { _forgeSummarySlug :: ForgeSlug,
    _forgeSummaryKind :: Text,
    _forgeSummaryWebUrl :: Text,
    -- | @configured@ or @registered@ (through the UI).
    _forgeSummarySource :: ForgeSource,
    -- | What to call it: the host of its web URL.
    _forgeSummaryName :: Text,
    -- | @active@, or @disabled@ for a registered forge the caller may bring
    -- back.
    _forgeSummaryStatus :: SummaryStatus,
    -- | Whether the caller may replace its secret and disable it
    -- ('managementRefusal'), so that the UI offers only what it may do.
    _forgeSummaryCanManage :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ForgeSummary where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

-- | A forge's status as 'renderForgeStatus' writes it.
newtype SummaryStatus = SummaryStatus ForgeStatus
  deriving stock (Eq, Show)

instance ToJSON SummaryStatus where
  toJSON (SummaryStatus status') = toJSON (renderForgeStatus status')

-- | A forge as anybody sees it: active, and managed by nobody.
-- 'forgeSummaries' sets the rest by field. The source has no safe default,
-- so it is always given.
plainSummary :: ForgeSource -> ForgeSlug -> ForgeKind -> Text -> ForgeSummary
plainSummary source slug' kind webUrl' =
  ForgeSummary
    { _forgeSummarySlug = slug',
      _forgeSummaryKind = case kind of
        GithubForgeKind -> "github"
        GiteaForgeKind -> "gitea",
      _forgeSummaryWebUrl = webUrl',
      _forgeSummarySource = source,
      _forgeSummaryName = fromMaybe webUrl' (hostOf webUrl'),
      _forgeSummaryStatus = SummaryStatus ForgeActive,
      _forgeSummaryCanManage = False
    }

-- | @GET /api/forges@: every forge garnix is active on, and, to whoever may
-- manage them, the disabled registered forges they may re-enable.
forgesAPI :: AuthResult AuthJwtPayload -> M [ForgeSummary]
forgesAPI authResult = do
  manager' <- managingAccount authResult
  active <- listActiveForges
  configured <- view #forges
  -- Registered rows are read only for a caller who may manage some: one
  -- query for the active ones, one lookup per candidate for disabled ones.
  (activeRows, disabledRows) <- case manager' of
    Nothing -> pure ([], [])
    Just account ->
      (,)
        <$> DB.listActiveRegisteredForges
        <*> (catMaybes <$> mapM DB.getRegisteredForge (disabledCandidates configured (map (_forgeConfigSlug . _forgeInstanceConfig . snd) active) account))
  pure $ forgeSummaries manager' active activeRows disabledRows

-- | The active forges, each saying whether the manager may manage it, then
-- the disabled registered forges among the rows that the manager may bring
-- back. Without a manager nobody may manage anything.
forgeSummaries :: Maybe User -> [(ForgeSource, ForgeInstance)] -> [DB.RegisteredForgeRow] -> [DB.RegisteredForgeRow] -> [ForgeSummary]
forgeSummaries manager' active activeRows disabledRows =
  [ (plainSummary source slug' (_forgeConfigKind config) (_forgeConfigWebUrl config))
      { _forgeSummaryCanManage = source == Registered && any (\row -> DB.rowSlug row == slug' && mayManage row) activeRows
      }
  | (source, instance') <- active,
    let config = _forgeInstanceConfig instance'
        slug' = _forgeConfigSlug config
  ]
    <> [ (plainSummary Registered (DB.rowSlug row) GiteaForgeKind (DB.rowWebUrl row))
           { _forgeSummaryStatus = SummaryStatus ForgeDisabled,
             _forgeSummaryCanManage = True
           }
       | row <- disabledRows,
         DB.rowStatus row == ForgeDisabled,
         mayManage row
       ]
  where
    mayManage row = any (isNothing . managementRefusal ReplaceSecret row) manager'

-- | Where a disabled registered forge the account may manage can be: a
-- disabled forge is found through the account's own identity on it, which it
-- keeps while the forge is disabled. So: the slugs of its identities on no
-- active forge, and on no configured forge's host.
disabledCandidates :: Map ForgeSlug ForgeInstance -> [ForgeSlug] -> User -> [ForgeSlug]
disabledCandidates configured activeSlugs account =
  [ slug'
  | identity <- account ^. identities,
    let slug' = identity ^. forge,
    slug' `notElem` activeSlugs,
    isNothing (configuredOnHost (getForgeSlug slug') configured)
  ]

-- | The account of a browser session, with all its identities, as
-- 'managedForge' judges it; 'Nothing' for an api token, which manages no
-- forge, for anybody logged out, and while registration is off.
managingAccount :: AuthResult AuthJwtPayload -> M (Maybe User)
managingAccount authResult = do
  registration <- view #forgeRegistration
  case (registration, authResult) of
    (Just _, Authenticated (WebSession _)) ->
      sessionUserOf authResult >>= \case
        Nothing -> pure Nothing
        Just session -> DB.getUserById (session ^. id)
    _ -> pure Nothing

-- | @POST /api/auth/start@: reads what 'startDecision' needs, and answers
-- it.
authStartAPI :: SockAddr -> Maybe Text -> AuthStartRequest -> M (Headers '[Header "Set-Cookie" SetCookie] AuthStartAnswer)
authStartAPI peer forwardedFor (AuthStartRequest url) = do
  registration <- view #forgeRegistration
  forM_ registration $ \registration' -> limited (_forgeRegistrationStartLimit registration') peer forwardedFor
  configured <- configuredForHost (hostOf url)
  registered <- case (configured, registration) of
    (Nothing, Just registration') -> do
      url' <- either (throw . BadRequest) pure $ normaliseRegistrationUrl (_forgeRegistrationAllowHttp registration') url
      let slug' = ForgeSlug (registrationHost url')
      DB.purgeStalePendingForges
      Just . (slug',) . fmap DB.rowStatus <$> DB.getRegisteredForge slug'
    _ -> pure Nothing
  case startDecision configured (isJust registration) registered of
    LoginThrough slug' -> startLogin slug'
    RegisterAt slug' -> noHeader . StartRegister slug' <$> callbackUrl slug'
    UnknownForge ->
      throw
        $ NotFoundWithMessage
        $ "This garnix does not know the forge at "
        <> url
        <> ". Whoever runs it can add it to services.garnixServer.forges."

-- | What @POST /api/auth/start@ answers.
data StartDecision = LoginThrough ForgeSlug | RegisterAt ForgeSlug | UnknownForge
  deriving stock (Eq, Show)

-- | What to answer for a forge URL, from the configured forge on its host,
-- whether registration is on, and the slug a registration of the URL would
-- have, with the status of the registration under it, if any. A configured
-- forge is answered whatever its URL looks like: the operator may serve it
-- over http or under a sub-path.
startDecision :: Maybe ForgeSlug -> Bool -> Maybe (ForgeSlug, Maybe ForgeStatus) -> StartDecision
startDecision configured registrationOn registered = case (configured, registrationOn, registered) of
  (Just slug', _, _) -> LoginThrough slug'
  (Nothing, False, _) -> UnknownForge
  (Nothing, True, Nothing) -> UnknownForge
  (Nothing, True, Just (slug', status')) -> case status' of
    Nothing -> RegisterAt slug'
    Just ForgeActive -> LoginThrough slug'
    -- A pending registration is replaced, a disabled forge is registered
    -- again, like an unknown one.
    Just ForgePending -> RegisterAt slug'
    Just ForgeDisabled -> RegisterAt slug'

-- | @POST /api/forges@: probes the URL, and stores the forge as pending until
-- an OAuth through it succeeds. Answers where to start that OAuth, and gives
-- the browser a cookie with the token that lets it, and only it, complete the
-- registration.
registerForgeAPI ::
  SockAddr ->
  Maybe Text ->
  Maybe Text ->
  RegisterForgeRequest ->
  M (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthStartAnswer)
registerForgeAPI peer forwardedFor cookies request = do
  registration <- view #forgeRegistration >>= maybe (throw NotFound) pure
  limited (_forgeRegistrationRegisterLimit registration) peer forwardedFor
  url' <- either (throw . BadRequest) pure $ normaliseRegistrationUrl (_forgeRegistrationAllowHttp registration) (registerUrl request)
  let slug' = ForgeSlug (registrationHost url')
      webUrl' = registrationWebUrl url'
      apiUrl' = webUrl' <> "/api/v1"
      clientId = T.strip (registerClientId request)
      clientSecret = T.strip (registerClientSecret request)
      presentedHash = hashRegistrationToken <$> registrationTokenFromCookies slug' cookies
  when (T.null clientId || T.null clientSecret) $ throw $ BadRequest "the OAuth client id and secret must not be empty"
  configuredForHost (Just $ registrationHost url') >>= \case
    Just _ -> throw $ ConflictWithMessage $ getForgeSlug slug' <> " is already configured on this garnix; log in through it"
    Nothing -> pure ()
  DB.purgeStalePendingForges
  -- Checked before probing, so that a refused registration costs the forge
  -- no request; the upsert checks again, atomically. The rows' times are the
  -- database's, and so is the time they are compared with.
  now <- DB.databaseNow
  refusal <- registrationRefusal slug' presentedHash now <$> DB.getRegisteredForge slug'
  forM_ refusal $ throw . ConflictWithMessage
  probeGitea (_forgeRegistrationManager registration) apiUrl'
  encryptedSecret <- encryptSecret clientSecret
  webhookSecret' <- encryptSecret =<< randomBase64 32
  token <- randomBase64 32
  stored <-
    DB.upsertPendingForge
      DB.PendingForge
        { pendingSlug = slug',
          pendingWebUrl = webUrl',
          pendingApiUrl = apiUrl',
          pendingOAuthClientId = clientId,
          pendingOAuthClientSecret = encryptedSecret,
          pendingWebhookSecret = webhookSecret',
          pendingTokenHash = hashRegistrationToken token
        }
      presentedHash
  unless stored $ do
    now' <- DB.databaseNow
    refusal' <- registrationRefusal slug' presentedHash now' <$> DB.getRegisteredForge slug'
    throw $ ConflictWithMessage $ fromMaybe (getForgeSlug slug' <> " was registered meanwhile; log in through it") refusal'
  log Notice $ "registered the forge " <> getForgeSlug slug' <> " (" <> webUrl' <> "), pending its first login"
  transport <- view #cookieSettings
  answer <- startLogin slug'
  pure
    $ addHeader
      defaultSetCookie
        { setCookieName = T.encodeUtf8 (registrationCookieName slug'),
          setCookieValue = T.encodeUtf8 token,
          -- Where the session cookie goes, as the OAuth state cookie, so
          -- that it reaches the login callback under any path prefix.
          setCookiePath = Just $ fromMaybe "/" $ cookiePath transport,
          setCookieMaxAge = Just (realToFrac pendingForgeTtl),
          setCookieHttpOnly = True,
          setCookieSecure = cookieIsSecure transport == Secure,
          setCookieSameSite = Just sameSiteLax
        }
      answer

-- | Why a registration of the slug cannot be stored now, if it cannot: the
-- forge is active, or a registration another browser submitted holds it
-- ('pendingForgeLock').
registrationRefusal :: ForgeSlug -> Maybe Text -> UTCTime -> Maybe DB.RegisteredForgeRow -> Maybe Text
registrationRefusal slug' presentedHash now = \case
  Nothing -> Nothing
  Just row -> case DB.rowStatus row of
    ForgeActive -> Just $ getForgeSlug slug' <> " is already registered; log in through it"
    ForgePending
      | isNothing presentedHash || presentedHash /= DB.rowRegistrationTokenHash row,
        now < lockedUntil ->
          Just
            $ "a registration of "
            <> getForgeSlug slug'
            <> " is already in progress; try again after "
            <> cs (formatTime defaultTimeLocale "%H:%M UTC" lockedUntil)
      | otherwise -> Nothing
    ForgeDisabled -> Nothing
    where
      lockedUntil = addUTCTime pendingForgeLock (DB.rowCreatedAt row)

-- | @PUT /api/forges/:slug/secret@. On a disabled forge it re-enables it at
-- once, with everyone who logged in through it.
replaceForgeSecretAPI :: ForgeSlug -> AuthResult AuthJwtPayload -> ReplaceForgeSecretRequest -> M ()
replaceForgeSecretAPI slug' authResult (ReplaceForgeSecretRequest clientSecret) = do
  row <- managedForge ReplaceSecret slug' authResult
  when (T.null $ T.strip clientSecret) $ throw $ BadRequest "the OAuth client secret must not be empty"
  DB.replaceForgeClientSecret slug' =<< encryptSecret (T.strip clientSecret)
  when (DB.rowStatus row == ForgeDisabled) $ log Notice $ "re-enabled the registered forge " <> getForgeSlug slug'

-- | @DELETE /api/forges/:slug@: disables the forge, keeping its history.
removeForgeAPI :: ForgeSlug -> AuthResult AuthJwtPayload -> M ()
removeForgeAPI slug' authResult = do
  void $ managedForge Disable slug' authResult
  DB.disableForge slug'

-- | The registered forge the caller may manage. Only from a browser: an api
-- token, which may leak from a CI log, manages no forge.
managedForge :: Management -> ForgeSlug -> AuthResult AuthJwtPayload -> M DB.RegisteredForgeRow
managedForge management slug' authResult = do
  void $ view #forgeRegistration >>= maybe (throw NotFound) pure
  -- A configured forge is the operator's, whoever asks.
  configured <- isJust . configuredOnHost (getForgeSlug slug') <$> view #forges
  when configured $ throw NotFound
  session <- webSessionUser authResult
  -- The session holds only identities on active forges; managing a disabled
  -- forge takes the account's identity on it.
  account <- DB.getUserById (session ^. id) >>= maybe (throw Unauthorized) pure
  row <- DB.getRegisteredForge slug' >>= maybe (throw NotFound) pure
  forM_ (managementRefusal management row account) throw
  pure row

-- | What is done to a registered forge.
data Management
  = -- | Replacing its client secret, which re-enables a disabled one.
    ReplaceSecret
  | Disable
  deriving stock (Eq, Show)

-- | Why the account, with all its identities, may not manage the registered
-- forge of the row, if it may not: whoever registered it, or an identity the
-- forge calls an administrator ('mayManageRegisteredForge'), may manage an
-- active forge, and re-enable a disabled one with a new secret.
managementRefusal :: Management -> DB.RegisteredForgeRow -> User -> Maybe Error
managementRefusal management row account = case (DB.rowStatus row, management) of
  (ForgePending, _) ->
    Just
      $ ConflictWithMessage
      $ getForgeSlug slug'
      <> " is still pending its first login; register it again instead"
  (ForgeDisabled, Disable) -> Just NotFound
  (ForgeDisabled, ReplaceSecret) -> byManager
  (ForgeActive, _) -> byManager
  where
    slug' = DB.rowSlug row
    byManager
      | mayManageRegisteredForge slug' (DB.rowRegisteredBy row) (account ^. id) (identityOn slug' account) = Nothing
      | otherwise =
          Just
            $ ForbiddenWithMessage
            $ "only whoever registered "
            <> getForgeSlug slug'
            <> ", or an administrator of it, may manage it"

-- * Helpers

limited :: RateLimiter -> SockAddr -> Maybe Text -> M ()
limited limiter peer forwardedFor =
  unlessM (allowRequest limiter (clientAddress peer forwardedFor)) $ throw TooManyRequests

-- | Whom to count requests against: the peer, or for a request that came
-- through a proxy on a non-public address (on the same host, or a load
-- balancer in the same network), the client that proxy appended to
-- @X-Forwarded-For@ (the last entry, without a port; earlier ones are
-- whatever the client claimed). An IPv4 client counts as itself however it
-- arrives, mapped into IPv6 by a dual-stack listener too. An IPv6 client
-- counts as its /64, the least a host is given, so that walking through its
-- own addresses does not reset its count.
clientAddress :: SockAddr -> Maybe Text -> Text
clientAddress peer forwardedFor
  | viaProxy,
    Just client <- forwardedFor >>= listToMaybe . reverse . filter (not . T.null) . map (withoutPort . T.strip) . T.splitOn "," =
      keyOf client
  | otherwise = keyOf peerHost
  where
    (viaProxy, peerHost) = case peer of
      SockAddrInet _ addr ->
        let (a, b, c, d) = hostAddressToTuple addr
         in (not (isPublicAddress peer), T.intercalate "." (map show [a, b, c, d]))
      SockAddrInet6 _ _ addr _ ->
        ( not (isPublicAddress peer),
          T.intercalate ":" $ map (cs . flip showHex "") $ (\(a, b, c, d, e, f, g, h) -> [a, b, c, d, e, f, g, h]) $ hostAddress6ToTuple addr
        )
      SockAddrUnix path -> (True, cs path)
    keyOf address = case ipv6Groups address of
      Just [0, 0, 0, 0, 0, 0xffff, high, low] -> T.intercalate "." $ map show [high `div` 256, high `mod` 256, low `div` 256, low `mod` 256]
      Just groups -> prefix64 groups
      Nothing -> address
    prefix64 groups = T.intercalate ":" (map (cs . flip showHex "") (take 4 groups)) <> "::/64"

-- | An address as a proxy may write it, without its port: @[v6]:port@ or
-- @v4:port@. A bare IPv6 address is left alone.
withoutPort :: Text -> Text
withoutPort entry
  | Just bracketed <- T.stripPrefix "[" entry = T.takeWhile (/= ']') bracketed
  | [host, _] <- T.splitOn ":" entry = host
  | otherwise = entry

-- | The eight groups of a textual IPv6 address, @::@ expanded, and a dotted
-- IPv4 tail (@::ffff:a.b.c.d@) as its two groups. Not an IPv6 address (an
-- IPv4 one, say): 'Nothing'.
ipv6Groups :: Text -> Maybe [Word16]
ipv6Groups raw = do
  let address = withDottedTail $ T.dropAround (`elem` ['[', ']']) (T.strip raw)
  guard $ T.any (== ':') address
  groups <- case T.splitOn "::" address of
    [whole] -> parts whole
    [before, after] -> do
      before' <- parts before
      after' <- parts after
      let missing = 8 - length before' - length after'
      guard (missing >= 1)
      pure $ before' <> replicate missing 0 <> after'
    _ -> Nothing
  guard (length groups == 8)
  pure groups
  where
    parts text'
      | T.null text' = Just []
      | otherwise = traverse group (T.splitOn ":" text')
    group text' = case readHex (cs text') of
      [(value, "")] | T.length text' <= 4 -> Just (fromInteger value)
      _ -> Nothing
    withDottedTail address = case T.breakOnEnd ":" address of
      (front, tail')
        | not (T.null front),
          Just [a, b, c, d] <- traverse octet (T.splitOn "." tail') ->
            front <> cs (showHex (a * 256 + b) "") <> ":" <> cs (showHex (c * 256 + d) "")
      _ -> address
    octet :: Text -> Maybe Int
    octet text' = do
      value <- readMaybe (cs text')
      value <$ guard (T.all isDigit text' && not (T.null text') && value <= 255)

hostOf :: Text -> Maybe Text
hostOf url = do
  authority <- uriAuthority =<< parseAbsoluteURI (cs $ T.strip url)
  pure $ T.dropWhileEnd (== '.') $ T.toLower $ cs $ uriRegName authority

-- | The configured forge that takes a host ('configuredOnHost').
configuredForHost :: Maybe Text -> M (Maybe ForgeSlug)
configuredForHost Nothing = pure Nothing
configuredForHost (Just host) = configuredOnHost host <$> view #forges

-- | The answer to log in through a forge: its authorize link, and the cookie
-- of the OAuth state that lets this browser, and only it, complete the login
-- (see 'Garnix.API.Auth.login').
startLogin :: ForgeSlug -> M (Headers '[Header "Set-Cookie" SetCookie] AuthStartAnswer)
startLogin slug' = do
  nonce <- newOAuthNonce
  link <- authorizeLink slug' (getOAuthNonce nonce)
  cookie <- oauthStateCookie nonce
  pure $ addHeader cookie $ StartLogin link

callbackUrl :: ForgeSlug -> M Text
callbackUrl slug' = do
  fromRelativeUrl <- relativeUrlConverter
  pure $ fromRelativeUrl $ oauthCallbackPath slug'

-- | Asks @/api/v1/version@, which Gitea and Forgejo both answer, through the
-- guarded manager.
probeGitea :: Manager -> Text -> M ()
probeGitea manager' apiUrl' = do
  let options =
        Wreq.defaults
          & Wreq.manager
          .~ Right manager'
          & Wreq.header "Accept"
          .~ ["application/json"]
          & Wreq.checkResponse
          ?~ (\_ _ -> pure ())
      notGitea reason =
        throw $ BadRequest $ apiUrl' <> "/version does not answer like a Gitea or Forgejo instance: " <> reason
  result <- liftIO $ Control.Exception.try $ Wreq.getWith options (cs $ apiUrl' <> "/version")
  case result of
    Left e -> notGitea $ describeFailure e
    Right response
      | response ^. Wreq.responseStatus . Wreq.statusCode /= 200 ->
          notGitea $ "it answered " <> show (response ^. Wreq.responseStatus . Wreq.statusCode)
      | isNothing (response ^? Wreq.responseBody . key "version" . _String) -> notGitea "its answer has no version"
      | otherwise -> pure ()
  where
    -- The guard's refusal arrives as is, or wrapped by http-client.
    describeFailure :: Control.Exception.SomeException -> Text
    describeFailure e
      | Just (ForbiddenAddress host _) <- Control.Exception.fromException e = notPublic host
      | Just (HttpExceptionRequest _ (ConnectionFailure inner)) <- Control.Exception.fromException e,
        Just (ForbiddenAddress host _) <- Control.Exception.fromException inner =
          notPublic host
      | Just reason <- guardLimit e = reason
      | Just (HttpExceptionRequest _ (InternalException inner)) <- Control.Exception.fromException e,
        Just reason <- guardLimit inner =
          reason
      | Just (HttpExceptionRequest _ content) <- Control.Exception.fromException e = "could not reach it (" <> show content <> ")"
      | Just (InvalidUrlException _ reason) <- Control.Exception.fromException e = cs reason
      | otherwise = "could not reach it"
    notPublic host = cs host <> " resolves to an address that is not public"
    guardLimit :: Control.Exception.SomeException -> Maybe Text
    guardLimit e
      | Just PlainHttpRefused <- Control.Exception.fromException e = Just "it redirected to plain http"
      | Just (GuardLimitExceeded reason) <- Control.Exception.fromException e = Just reason
      | otherwise = Nothing
