module Garnix.API.Account where

import Control.Concurrent.Async.Lifted
import Control.Lens
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Garnix.AccessToken
import Garnix.AccessToken.Types
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.GithubInterface.Types
import Garnix.GithubUserToken
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types hiding (Admin, installationId)
import Servant.Auth.Server

data AccountAPI route = AccountAPI
  { _accountAPIUsage :: route :- "usage" :> Get '[JSON] UsageOverview,
    _accountAPIOrgUsage :: route :- "usage" :> Capture "org" GhRepoOwner :> Get '[JSON] OrgUsage,
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
  { _orgUsageCiTime :: Duration,
    _orgUsagePrDeploymentTime :: Duration,
    _orgUsageInstallationStatus :: InstallationStatus
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OrgUsage where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

getOwnersWithVisibleInstallation :: GhToken -> M (Set.Set GhRepoOwner)
getOwnersWithVisibleInstallation token = do
  installationIds <- getInstallations token
  repos <- forConcurrently installationIds $ \installationId ->
    getReposInInstallationAccessibleTo installationId token
  pure $ Set.fromList $ mapMaybe ownerOf $ mconcat repos
  where
    ownerOf fullName = case T.splitOn "/" fullName of
      [owner, _] -> Just $ GhRepoOwner $ GhLogin owner
      _ -> Nothing

getViewableOwners :: User -> GhToken -> M (Map.Map GhRepoOwner InstallationStatus)
getViewableOwners user token = do
  (memberships, installedOwners) <-
    concurrently
      (getInstalledOrgs token)
      (getOwnersWithVisibleInstallation token)
  let self = GhRepoOwner (user ^. githubLogin)
      adminOrgs = organizationName <$> filter (\membership -> role membership == Admin) memberships
      readableOrgs = Set.fromList $ organizationName <$> memberships
      opaqueOrgs = Set.delete self $ installedOwners `Set.difference` readableOrgs
      selfStatus =
        if self `Set.member` installedOwners
          then AppInstalled
          else AppNotInstalled
  pure
    $ Map.fromList
    $ (self, selfStatus)
    : [(org, AppInstalled) | org <- adminOrgs]
      <> [(org, AppInstalledWithoutMemberAccess) | org <- Set.toList opaqueOrgs]

getUsageForOrg :: Map.Map GhRepoOwner Duration -> GhRepoOwner -> InstallationStatus -> M OrgUsage
getUsageForOrg usage org installationStatus = do
  prDeploymentTime <- DB.getPrDeployDurationForOwner org
  pure
    OrgUsage
      { _orgUsageCiTime = fromMaybe emptyDuration $ Map.lookup org usage,
        _orgUsagePrDeploymentTime = prDeploymentTime,
        _orgUsageInstallationStatus = installationStatus
      }

usageOverview :: AuthResult AuthJwtPayload -> M UsageOverview
usageOverview (Authenticated (WebSession user)) = do
  owners <- withGithubUserToken user $ getViewableOwners user
  usage <- DB.getCurrentMonthUsages (Map.keys owners)
  UsageOverview <$> Map.traverseWithKey (getUsageForOrg usage) owners
usageOverview _ = throw Unauthorized

orgUsage :: AuthResult AuthJwtPayload -> GhRepoOwner -> M OrgUsage
orgUsage (Authenticated (WebSession user)) org = do
  owners <- withGithubUserToken user $ getViewableOwners user
  installationStatus <- maybe (throw NotFound) pure $ Map.lookup org owners
  usage <- DB.getCurrentMonthUsages [org]
  getUsageForOrg usage org installationStatus
orgUsage _ _ = throw Unauthorized

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
createAccessToken (Authenticated (WebSession user)) (CreateTokenRequestBody name (fromMaybe fallbackAccessTokenScopes -> scopes)) = do
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
enabledReposOf (Authenticated (WebSession user)) = withGithubUserToken user $ \ghToken -> do
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
