module Garnix.API where

import Autodocodec.Schema (JSONSchema)
import Data.Text qualified as T
import Garnix.API.Account
import Garnix.API.Auth
import Garnix.API.Badges
import Garnix.API.Builds
import Garnix.API.Cache
import Garnix.API.Commits
import Garnix.API.ConfigSchema (garnixConfigJsonSchema)
import Garnix.API.Dev (DevAPI, devAPI)
import Garnix.API.GhWebhooks
import Garnix.API.Health
import Garnix.API.Hosts (HostsAPI, hostsAPI)
import Garnix.API.Keys
import Garnix.API.Modules
import Garnix.API.Runs (RunAPI, runAPI)
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
    account :: r :- "api" :> "account" :> Auth '[JWT, Cookie] AuthJwtPayload :> ToServantApi AccountAPI,
    build :: r :- "api" :> "build" :> Auth '[JWT, Cookie] AuthJwtPayload :> ToServantApi BuildAPI,
    commit :: r :- "api" :> "commits" :> Auth '[JWT, Cookie] AuthJwtPayload :> ToServantApi CommitAPI,
    run :: r :- "api" :> "run" :> Auth '[JWT, Cookie] AuthJwtPayload :> ToServantApi RunAPI,
    modules :: r :- "api" :> "modules" :> Auth '[JWT, Cookie] AuthJwtPayload :> ToServantApi ModulesAPI,
    dev :: r :- "api" :> "dev" :> ToServantApi DevAPI,
    keys :: r :- "api" :> "keys" :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> "repo-key.public" :> Get '[PlainText] PublicKey,
    actionKeys :: r :- "api" :> "keys" :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> "actions" :> Capture "action" PackageName :> "key.public" :> Get '[PlainText] PublicKey,
    login :: r :- "api" :> "login" :> ToServantApi LoginAPI,
    signup :: r :- "api" :> "signup" :> ToServantApi SignupAPI,
    whoami :: r :- "api" :> "whoami" :> Auth '[JWT, Cookie] AuthJwtPayload :> Get '[JSON] (Maybe UserDto),
    authJwt :: r :- "api" :> "auth" :> "jwt" :> ToServantApi AuthJwtAPI,
    config :: r :- "api" :> "config" :> Get '[JSON] FrontendConfig,
    badges :: r :- "api" :> "badges" :> Capture "owner" GhRepoOwner :> Capture "repo" GhRepoName :> QueryParam "branch" Branch :> Get '[JSON] Badge,
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
      account = toServant . accountAPI,
      dev = devAPI,
      login = toServant loginAPI,
      signup = toServant signupAPI,
      whoami = whoAmIAPI,
      authJwt = toServant authJwtAPI,
      keys = Garnix.API.Keys.getRepoPublicKey,
      actionKeys = Garnix.API.Keys.getActionPublicKey,
      config = getConfig,
      build = toServant . buildAPI,
      commit = toServant . commitAPI,
      run = toServant . runAPI,
      modules = toServant . modulesAPI,
      badges = badgesAPI,
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

waitlistAPI :: Email -> M ()
waitlistAPI email = do
  let isValid = '@' `T.elem` getEmail email
  unless isValid $ throw InvalidEmail
  DB.addToWaitlist email
