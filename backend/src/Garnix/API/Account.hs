module Garnix.API.Account where

import Control.Concurrent.Async.Lifted
import Control.Lens
import Data.Map.Strict qualified as Map
import Data.Maybe
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Entitlements (getPlans, setExtraUsageLimits)
import Garnix.Entitlements qualified as Entitlements
import Garnix.GithubInterface.Types
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types hiding (Admin, installationId)
import Servant (Put)
import Servant.Auth.Server

data AccountAPI route = AccountAPI
  { _accountAPIUsage :: route :- "usage" :> Get '[JSON] UsageOverview,
    _accountAPIOrgUsage :: route :- "usage" :> Capture "org" GhRepoOwner :> Get '[JSON] OrgUsage,
    _accountAPISetUsageLimits :: route :- "usage" :> Capture "org" GhRepoOwner :> ReqBody '[JSON] ExtraUsageLimits :> Put '[JSON] NoContent,
    _accountAPIEnabledRepos :: route :- "repos" :> Get '[JSON] EnabledRepos,
    _accountAPIGetAccessTokens :: route :- "tokens" :> Get '[JSON] GetTokensResponseBody,
    _accountAPICreateAccessToken :: route :- "tokens" :> ReqBody '[JSON] CreateTokenRequestBody :> Post '[JSON] CreateTokenResponseBody,
    _accountAPIRevokeAccessToken :: route :- "tokens" :> Capture "tokenId" Int64 :> Delete '[JSON] NoContent
  }
  deriving (Generic)

accountAPI :: AuthResult AuthJwtPayload -> AccountAPI (AsServerT M)
accountAPI user =
  AccountAPI
    { _accountAPIUsage = usageOverview user,
      _accountAPIOrgUsage = orgUsage user,
      _accountAPISetUsageLimits = setUsageLimits user,
      _accountAPIEnabledRepos = enabledReposOf user,
      _accountAPIGetAccessTokens = getAccessTokens user,
      _accountAPICreateAccessToken = createAccessToken user,
      _accountAPIRevokeAccessToken = revokeAccessToken user
    }

data UsageOverview = UsageOverview
  { _usageOverviewByOrg :: Map.Map GhRepoOwner OrgUsage
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON UsageOverview where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

data OrgUsage = OrgUsage
  { _orgUsagePlan :: ProductPlan,
    _orgUsageCiTime :: Duration,
    _orgUsagePrDeploymentTime :: Duration,
    _orgUsageInstallationStatus :: InstallationStatus
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OrgUsage where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

getOrgsUserIsAdminIn :: User -> GhToken -> M [GhRepoOwner]
getOrgsUserIsAdminIn user token =
  (GhRepoOwner (user ^. githubLogin) :)
    . map organizationName
    . filter (\membership -> role membership == Admin)
    <$> getInstalledOrgs token

getUsageForOrg :: Map.Map GhRepoOwner Duration -> Map.Map GhRepoOwner ProductPlan -> GhRepoOwner -> M OrgUsage
getUsageForOrg usage plans org = do
  prDeploymentTime <- DB.getPrDeployDurationForOwner org
  plan <- case Map.lookup org plans of
    Just plan -> pure plan
    Nothing -> throw $ OtherError "Impossible: plan missing for org passed into getPlans"
  installationStatus <- DB.getInstallationStatus org
  pure
    OrgUsage
      { _orgUsagePlan = plan,
        _orgUsageCiTime = fromMaybe emptyDuration $ Map.lookup org usage,
        _orgUsagePrDeploymentTime = prDeploymentTime,
        _orgUsageInstallationStatus = installationStatus
      }

usageOverview :: AuthResult AuthJwtPayload -> M UsageOverview
usageOverview (Authenticated (WebSession user ghToken)) = do
  orgs <- getOrgsUserIsAdminIn user ghToken
  usage <- DB.getCurrentMonthUsages (GhRepoOwner (user ^. githubLogin) : orgs)
  plans <- getPlans orgs
  map <- mkMapM orgs $ getUsageForOrg usage plans
  pure $ UsageOverview map
usageOverview _ = throw Unauthorized

mkMapM :: (Monad m, Ord key) => [key] -> (key -> m value) -> m (Map.Map key value)
mkMapM keys f =
  Map.fromList
    <$> forM
      keys
      ( \key -> do
          value <- f key
          pure (key, value)
      )

orgUsage :: AuthResult AuthJwtPayload -> GhRepoOwner -> M OrgUsage
orgUsage (Authenticated (WebSession user ghToken)) org = do
  orgsUserIsAdminIn <- getOrgsUserIsAdminIn user ghToken
  when (org `notElem` orgsUserIsAdminIn) $ throw NotFound
  usage <- DB.getCurrentMonthUsages [org]
  plans <- getPlans [org]
  getUsageForOrg usage plans org
orgUsage _ _ = throw Unauthorized

setUsageLimits :: AuthResult AuthJwtPayload -> GhRepoOwner -> ExtraUsageLimits -> M NoContent
setUsageLimits (Authenticated (WebSession user ghToken)) org newLimits = do
  orgsUserIsAdminIn <- getOrgsUserIsAdminIn user ghToken
  when (org `notElem` orgsUserIsAdminIn) $ throw Unauthorized
  plan <- Entitlements.getPlan org
  unless (plan ^. isPaid) $ do
    throw $ BadRequest "User does not have an active plan"
  setExtraUsageLimits org newLimits
  pure NoContent
setUsageLimits _ _ _ = throw Unauthorized


data GetTokensResponseBody = GetTokensResponseBody
  { _getTokensResponseBodyTokens :: [AccessTokenMetadata]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GetTokensResponseBody where
  toJSON = ourToJSON

data CreateTokenRequestBody = CreateTokenRequestBody
  { _createTokenRequestBodyName :: Text,
    _createTokenRequestBodyScopes :: Maybe AccessTokenScopes
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON CreateTokenRequestBody where
  parseJSON = ourParseJSON

data CreateTokenResponseBody = CreateTokenResponseBody
  { _createTokenResponseBodyToken :: AccessToken
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON CreateTokenResponseBody where
  toJSON = ourToJSON

getAccessTokens :: AuthResult AuthJwtPayload -> M GetTokensResponseBody
getAccessTokens (Authenticated ((^. #user) -> user)) = GetTokensResponseBody <$> DB.getAccessTokensForUser (user ^. id)
getAccessTokens _ = throw Unauthorized

-- For backwards compatability during the first deploy, if `scopes` is not provided, we default to just a cache scope.
fallbackAccessTokenScopes :: AccessTokenScopes
fallbackAccessTokenScopes = AccessTokenScopes {api = False, cache = True}

createAccessToken :: AuthResult AuthJwtPayload -> CreateTokenRequestBody -> M CreateTokenResponseBody
createAccessToken (Authenticated (WebSession user _)) (CreateTokenRequestBody name (fromMaybe fallbackAccessTokenScopes -> scopes)) = do
  when (scopes == AccessTokenScopes {api = False, cache = False}) $ do
    throw $ BadRequest "no scopes enabled"
  accessToken <- generateToken (user ^. id) name scopes
  pure $ CreateTokenResponseBody accessToken
createAccessToken (Authenticated (ApiSession _)) _ = throw $ ForbiddenWithMessage "This endpoint is not available through the programmatic api."
createAccessToken _ _ = throw Unauthorized

revokeAccessToken :: AuthResult AuthJwtPayload -> Int64 -> M NoContent
revokeAccessToken (Authenticated ((^. #user) -> user)) tokenId = do
  DB.deleteAccessTokenForUser (user ^. id) tokenId
  pure NoContent
revokeAccessToken _ _ = throw Unauthorized

enabledReposOf :: AuthResult AuthJwtPayload -> M EnabledRepos
enabledReposOf (Authenticated (WebSession _ ghToken)) = do
  installationIds <- getInstallations ghToken
  repos <- forConcurrently installationIds $ \id ->
    getReposInInstallationAccessibleTo id ghToken
  return
    $ EnabledRepos
    $ mconcat repos
enabledReposOf _ = throw Unauthorized

data EnabledRepos = EnabledRepos {_enabledReposRepos :: [Text]}
  deriving stock (Eq, Show, Generic)

instance ToJSON EnabledRepos where
  toJSON = ourToJSON
