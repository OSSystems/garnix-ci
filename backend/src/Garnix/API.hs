module Garnix.API where

import Autodocodec.Schema (JSONSchema)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Garnix.API.Account
import Garnix.API.Auth
import Garnix.API.Badges
import Garnix.API.Builds
import Garnix.API.Cache
import Garnix.API.Commits
import Garnix.API.ConfigSchema (garnixConfigJsonSchema)
import Garnix.API.Dev (DevAPI, devAPI)
import Garnix.API.ForgeWebhooks (ForgeWebhookAPI, forgeWebhookAPI)
import Garnix.API.GhWebhooks
import Garnix.API.Health
import Garnix.API.Hosts (HostsAPI, hostsAPI)
import Garnix.API.Keys
import Garnix.API.Modules
import Garnix.API.Runs (RunAPI, runAPI)
import Garnix.Access (repoIdFromRoute)
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types
import Servant
import Servant.Auth.Server

type CT = '[JSON]

api :: Proxy (ToServant WholeAPI AsApi)
api = genericApi (Proxy :: Proxy WholeAPI)

data WholeAPI r = WholeAPI
  { events ::
      r
        :- "api"
          :> "events"
          :> "github"
          :> ToServantApi GhWebhookAPI,
    -- | Webhooks of the forge instances in @GARNIX_FORGES_FILE@.
    forgeEvents :: r :- "api" :> "forges" :> ForgeWebhookAPI,
    account :: r :- "api" :> "account" :> Auth '[JWT, Cookie] AuthJwtPayload :> ToServantApi AccountAPI,
    build :: r :- "api" :> "build" :> Auth '[JWT, Cookie] AuthJwtPayload :> ToServantApi BuildAPI,
    commit :: r :- "api" :> "commits" :> Auth '[JWT, Cookie] AuthJwtPayload :> ToServantApi CommitAPI,
    run :: r :- "api" :> "run" :> Auth '[JWT, Cookie] AuthJwtPayload :> ToServantApi RunAPI,
    modules :: r :- "api" :> "modules" :> Auth '[JWT, Cookie] AuthJwtPayload :> ToServantApi ModulesAPI,
    dev :: r :- "api" :> "dev" :> ToServantApi DevAPI,
    -- | The forge-less key and badge routes predate multi-forge support and
    -- name a github.com repository. They stay as aliases, not redirects:
    -- READMEs, shields.io and scripts call them and need not follow one.
    keys :: r :- "api" :> "keys" :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> "repo-key.public" :> Get '[PlainText] PublicKey,
    actionKeys :: r :- "api" :> "keys" :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> "actions" :> Capture "action" PackageName :> "key.public" :> Get '[PlainText] PublicKey,
    forgeKeys :: r :- "api" :> "keys" :> Capture "forge" ForgeSlug :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> "repo-key.public" :> Get '[PlainText] PublicKey,
    forgeActionKeys :: r :- "api" :> "keys" :> Capture "forge" ForgeSlug :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> "actions" :> Capture "action" PackageName :> "key.public" :> Get '[PlainText] PublicKey,
    login :: r :- "api" :> "login" :> ToServantApi LoginAPI,
    whoami :: r :- "api" :> "whoami" :> Auth '[JWT, Cookie] AuthJwtPayload :> Get '[JSON] (Maybe UserDto),
    authJwt :: r :- "api" :> "auth" :> "jwt" :> ToServantApi AuthJwtAPI,
    -- | Logging in through any configured forge instance. @login@ above is
    -- GitHub's, kept for the route its OAuth app calls back to.
    forgeLogin :: r :- "api" :> "auth" :> Capture "forge" ForgeSlug :> "login" :> ToServantApi LoginAPI,
    -- | Connecting the session's account to a forge, and disconnecting it.
    forgeConnect :: r :- "api" :> "auth" :> Capture "forge" ForgeSlug :> ToServantApi ConnectAPI,
    config :: r :- "api" :> "config" :> Get '[JSON] FrontendConfig,
    badges :: r :- "api" :> "badges" :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> QueryParam "branch" Branch :> Get '[JSON] Badge,
    forgeBadges :: r :- "api" :> "badges" :> Capture "forge" ForgeSlug :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> QueryParam "branch" Branch :> Get '[JSON] Badge,
    forges :: r :- "api" :> "forges" :> Get '[JSON] [ForgeSummary],
    waitlist :: r :- "api" :> "waitlist" :> ReqBody '[JSON] Email :> Post '[JSON] (),
    cache :: r :- "api" :> "cache" :> ToServantApi CacheAPI,
    garnixConfigSchema :: r :- "api" :> "garnix-config-schema.json" :> Get '[JSON] JSONSchema,
    health :: r :- "api" :> "health" :> ToServantApi HealthAPI,
    -- | Not behind @Auth@ at this level: most of these are consumed by the
    -- gateway and by deployed guests, which have no session. The two that do
    -- need a user carry their own @Auth@ (see "Garnix.API.Hosts").
    hosts :: r :- "api" :> "hosts" :> ToServantApi HostsAPI
  }
  deriving stock (Generic)

data ProjectAPI r = ProjectAPI
  { get ::
      r
        :- Capture "gh_owner" Text
          :> Capture "gh_repo" Text
          :> "commit"
          :> Capture "commit" CommitHash
          :> Get CT (),
    post ::
      r
        :- Capture "gh_owner" Text
          :> Capture "gh_repo" Text
          :> "commit"
          :> Capture "commit" CommitHash
          :> QueryParam "token" GhToken
          :> Post '[JSON] RunResult
  }
  deriving stock (Generic)

wholeAPI :: WholeAPI (AsServerT M)
wholeAPI =
  WholeAPI
    { events = toServant ghWebhookAPI,
      forgeEvents = forgeWebhookAPI,
      account = toServant . accountAPI,
      dev = devAPI,
      login = toServant (loginAPI githubForge),
      forgeLogin = toServant . loginAPI,
      forgeConnect = toServant . connectAPI,
      whoami = whoAmIAPI,
      authJwt = toServant authJwtAPI,
      keys = \owner name -> Garnix.API.Keys.getRepoPublicKey (RepoId githubForge owner name),
      actionKeys = \owner name -> Garnix.API.Keys.getActionPublicKey (RepoId githubForge owner name),
      forgeKeys = \slug owner name -> Garnix.API.Keys.getRepoPublicKey =<< repoIdFromRoute slug owner name,
      forgeActionKeys = \slug owner name action -> repoIdFromRoute slug owner name >>= \repo -> Garnix.API.Keys.getActionPublicKey repo action,
      config = getConfig,
      build = toServant . buildAPI,
      commit = toServant . commitAPI,
      run = toServant . runAPI,
      modules = toServant . modulesAPI,
      badges = \owner name -> badgesAPI (RepoId githubForge owner name),
      forgeBadges = \slug owner name branch' -> repoIdFromRoute slug owner name >>= \repo -> badgesAPI repo branch',
      forges = forgesAPI,
      waitlist = waitlistAPI,
      cache = toServant cacheAPI,
      garnixConfigSchema = pure garnixConfigJsonSchema,
      health = toServant healthAPI,
      hosts = toServant hostsAPI
    }

getConfig :: M FrontendConfig
getConfig = do
  ghAppName <- view #githubAppName
  pure $ FrontendConfig {_frontendConfigGithubAppName = ghAppName}

-- | A forge instance as the frontend sees it: enough to build URLs and links,
-- none of its secrets.
data ForgeSummary = ForgeSummary
  { _forgeSummarySlug :: ForgeSlug,
    _forgeSummaryKind :: Text,
    _forgeSummaryWebUrl :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ForgeSummary where
  toEncoding = ourToEncoding
  toJSON = ourToJSON

forgesAPI :: M [ForgeSummary]
forgesAPI = do
  configured <- view #forges
  pure $ map (summarize . _forgeInstanceConfig) $ Map.elems configured
  where
    summarize config =
      ForgeSummary
        { _forgeSummarySlug = _forgeConfigSlug config,
          _forgeSummaryKind = case _forgeConfigKind config of
            GithubForgeKind -> "github"
            GiteaForgeKind -> "gitea",
          _forgeSummaryWebUrl = _forgeConfigWebUrl config
        }

waitlistAPI :: Email -> M ()
waitlistAPI email = do
  let isValid = '@' `T.elem` getEmail email
  unless isValid $ throw InvalidEmail
  DB.addToWaitlist email
