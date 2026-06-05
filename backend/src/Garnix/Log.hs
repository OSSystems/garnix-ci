module Garnix.Log where

import Garnix.Prelude
import Garnix.Types
import GitHub.App.Auth (InstallationAuth)

class Loggable a where
  asLog :: a -> [(Text, Text)]

instance Loggable PackageType where
  asLog n = [("package_type", showPretty n)]

instance Loggable PackageName where
  asLog n = [("package", prj n)]

instance Loggable Branch where
  asLog n = [("branch", prj n)]

instance Loggable RepoPublicity where
  asLog (RepoIsPublic n) = [("public", show n)]

instance Loggable PrFromFork where
  asLog (PrFromFork n) = [("fork", n)]

instance (Loggable a) => Loggable (Maybe a) where
  asLog = maybe [] asLog

instance Loggable MaybeSystem where
  asLog n = case n of
    IsSystem s -> [("system", s ^. systemTextIso)]
    NoSystem -> []

instance Loggable CommitHash where
  asLog (CommitHash n) = [("commit", n)]

instance Loggable GhRepoOwner where
  asLog (GhRepoOwner (GhLogin n)) = [("gh_owner", n)]

instance Loggable GhRepoName where
  asLog (GhRepoName n) = [("gh_repo", n)]

instance Loggable GhPullRequestId where
  asLog (GhPullRequestId n) = [("gh_pr", show n)]

instance Loggable UserId where
  asLog (UserId n) = [("user", show n)]

instance Loggable GhLogin where
  asLog (GhLogin n) = [("req_user", n)]

instance Loggable InstallationAuth where
  asLog _ = []

instance Loggable BuildId where
  asLog id = [("buildId", getHashId hash <> "(" <> show (hash ^. hashIdInt) <> ")")]
    where
      hash = getBuildId id

instance Loggable CommitInfo where
  asLog info = asLog (info ^. _CommitInfo)

instance Loggable RepoInfo where
  asLog info = asLog (info ^. _RepoInfo)

instance Loggable PackageInfo where
  asLog info = asLog (info ^. _PackageInfo)

instance Loggable GhToken where
  asLog _ = []

instance
  (Loggable a, Loggable b, Loggable c) =>
  Loggable (a, b, c)
  where
  asLog (a, b, c) =
    asLog a
      <> asLog b
      <> asLog c

instance
  (Loggable a, Loggable b, Loggable c, Loggable d) =>
  Loggable (a, b, c, d)
  where
  asLog (a, b, c, d) =
    asLog a
      <> asLog b
      <> asLog c
      <> asLog d

instance
  (Loggable a, Loggable b, Loggable c, Loggable d, Loggable e, Loggable f) =>
  Loggable (a, b, c, d, e, f)
  where
  asLog (a, b, c, d, e, f) =
    asLog a
      <> asLog b
      <> asLog c
      <> asLog d
      <> asLog e
      <> asLog f

instance
  (Loggable a, Loggable b, Loggable c, Loggable d, Loggable e, Loggable f, Loggable g, Loggable h) =>
  Loggable (a, b, c, d, e, f, g, h)
  where
  asLog (a, b, c, d, e, f, g, h) =
    asLog a
      <> asLog b
      <> asLog c
      <> asLog d
      <> asLog e
      <> asLog f
      <> asLog g
      <> asLog h
