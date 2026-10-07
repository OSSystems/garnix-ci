-- | Gitea (and Forgejo) webhook deliveries: checking their signature and
-- reading the events garnix acts on.
--
-- Everything here is pure; "Garnix.API.ForgeWebhooks" serves the route.
module Garnix.Forge.Gitea.Webhook
  ( verifyGiteaSignature,
    GiteaWebhook (..),
    parseGiteaWebhook,
    giteaWebhookRepo,
    giteaForgeEvent,
  )
where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (Object, Parser, parseEither, withObject, (.:), (.:?))
import Data.Bits (xor, (.|.))
import Data.ByteString.Lazy qualified as BSL
import Data.Char (ord, toLower)
import Data.Text qualified as T
import Garnix.Orchestrator (ForgeEvent (..))
import Garnix.Prelude
import Garnix.Types

-- | Whether a delivery was signed with the instance's webhook secret. Gitea
-- sends the hex HMAC-SHA256 of the raw body, without prefix, in
-- @X-Gitea-Signature@ (Forgejo also in @X-Forgejo-Signature@). An empty
-- secret or a missing signature never verifies.
verifyGiteaSignature ::
  -- | The webhook secret
  StrictByteString ->
  -- | The signature header, if any
  Maybe Text ->
  -- | The raw body, exactly as received
  LazyByteString ->
  Bool
verifyGiteaSignature secret signature body
  | secret == mempty = False
  | otherwise = case signature of
      Nothing -> False
      Just given -> constantTimeEq (T.toLower $ T.strip given) expected
  where
    expected :: Text
    expected = show $ hmacGetDigest (hmac secret (BSL.toStrict body) :: HMAC SHA256)

-- | Compares two strings in time that depends only on their lengths, so that
-- a forged signature cannot be found byte by byte.
constantTimeEq :: Text -> Text -> Bool
constantTimeEq a b =
  T.length a
    == T.length b
    && foldl' (\acc (x, y) -> acc .|. (ord x `xor` ord y)) 0 (T.zip a b)
    == 0

-- | The part of a delivery garnix acts on.
data GiteaWebhook
  = -- | A branch or tag now points at a commit.
    GiteaPush
      { _giteaWebhookRepo :: RepoId,
        _giteaWebhookPublicity :: RepoPublicity,
        _giteaWebhookSender :: GhLogin,
        _giteaWebhookBranch :: Maybe Branch,
        _giteaWebhookCommit :: CommitHash
      }
  | -- | A pull request was opened, or its head moved.
    GiteaPullRequest
      { _giteaWebhookRepo :: RepoId,
        _giteaWebhookPublicity :: RepoPublicity,
        _giteaWebhookSender :: GhLogin,
        _giteaWebhookPrFromFork :: Maybe PrFromFork,
        _giteaWebhookCommit :: CommitHash,
        _giteaWebhookNumber :: GhPullRequestId
      }
  deriving stock (Eq, Show)

giteaWebhookRepo :: GiteaWebhook -> RepoId
giteaWebhookRepo = _giteaWebhookRepo

-- | Reads a delivery of the given event type (the @X-Gitea-Event@ header).
-- 'Nothing' for deliveries garnix ignores: other events, other pull request
-- actions, and pushes that delete a ref.
parseGiteaWebhook :: ForgeSlug -> Text -> LazyByteString -> Either String (Maybe GiteaWebhook)
parseGiteaWebhook slug' event body = case event of
  "push" -> withPayload parsePush
  "pull_request" -> withPayload parsePullRequest
  _ -> Right Nothing
  where
    withPayload :: (Object -> Parser (Maybe GiteaWebhook)) -> Either String (Maybe GiteaWebhook)
    withPayload parser =
      eitherDecode body >>= parseEither (withObject (cs event <> " payload") parser)

    parsePush o = do
      after <- o .: "after"
      if T.all (== '0') after
        then pure Nothing
        else do
          ref <- o .: "ref"
          (repo, publicity) <- o .: "repository" >>= parseRepository
          sender <- o .: "sender" >>= parseLogin
          pure
            $ Just
            $ GiteaPush
              { _giteaWebhookRepo = repo,
                _giteaWebhookPublicity = publicity,
                _giteaWebhookSender = sender,
                _giteaWebhookBranch = case T.stripPrefix "refs/heads/" ref of
                  Just branch' | not (T.null branch') -> Just $ Branch branch'
                  _ -> Nothing,
                _giteaWebhookCommit = CommitHash after
              }

    parsePullRequest o = do
      action :: Text <- o .: "action"
      -- Gitea says "synchronized" where GitHub says "synchronize".
      if action `notElem` ["opened", "synchronized"]
        then pure Nothing
        else do
          (repo, publicity) <- o .: "repository" >>= parseRepository
          sender <- o .: "sender" >>= parseLogin
          number' <- o .: "number"
          pr <- o .: "pull_request"
          head' <- pr .: "head"
          sha' <- head' .: "sha"
          headRepo <- head' .:? "repo" >>= maybe (fail "pull request head without repo") pure
          headFullName <- headRepo .: "full_name"
          baseFullName <- o .: "repository" >>= (.: "full_name")
          pure
            $ Just
            $ GiteaPullRequest
              { _giteaWebhookRepo = repo,
                _giteaWebhookPublicity = publicity,
                _giteaWebhookSender = sender,
                _giteaWebhookPrFromFork =
                  if T.map toLower headFullName /= T.map toLower baseFullName
                    then Just $ PrFromFork headFullName
                    else Nothing,
                _giteaWebhookCommit = CommitHash sha',
                _giteaWebhookNumber = GhPullRequestId number'
              }

    parseRepository :: Object -> Parser (RepoId, RepoPublicity)
    parseRepository repo = do
      owner <- repo .: "owner" >>= parseLogin
      name' <- repo .: "name"
      private <- repo .: "private"
      pure (RepoId slug' (GhRepoOwner owner) (GhRepoName name'), RepoIsPublic $ not private)

    parseLogin :: Object -> Parser GhLogin
    parseLogin user = GhLogin <$> user .: "login"

-- | What garnix does about a delivery, given the repository's credentials.
giteaForgeEvent :: RepoInfo -> GiteaWebhook -> ForgeEvent
giteaForgeEvent repoInfo' hook = case hook of
  GiteaPush {_giteaWebhookBranch} ->
    CommitPushed False $ commitInfo _giteaWebhookBranch Nothing
  GiteaPullRequest {_giteaWebhookPrFromFork, _giteaWebhookNumber} ->
    PullRequestUpdated (commitInfo Nothing _giteaWebhookPrFromFork) _giteaWebhookNumber
  where
    commitInfo branch' prFromFork' =
      CommitInfo
        { _commitInfoReqUser = ForgeLogin (repoInfo' ^. repoId . forge) (_giteaWebhookSender hook),
          _commitInfoRepoPublicity = _giteaWebhookPublicity hook,
          _commitInfoRepoInfo = repoInfo',
          _commitInfoBranch = branch',
          _commitInfoPrFromFork = prFromFork',
          _commitInfoCommit = _giteaWebhookCommit hook
        }
