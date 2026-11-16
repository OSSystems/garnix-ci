-- | The state behind cancelling the work of superseded commits. Split from
-- "Garnix.Supersede" so that 'Garnix.Monad.Env' can hold it.
module Garnix.Supersede.Types
  ( Line (..),
    Supersession (..),
    LineRun (..),
    BuildProgress (..),
    NeededClosure (..),
  )
where

import Control.Concurrent (MVar)
import Control.Concurrent.STM (TVar)
import Data.Map.Strict (Map)
import Garnix.BuildLogs.Internal (InternalId)
import Garnix.Nix.Types qualified as Nix
import Garnix.Prelude
import Garnix.Types (Branch, CommitHash, GhPullRequestId, RepoId)

-- | A line of commits where a new head supersedes the older ones: a branch,
-- or a pull request from a fork, which has no branch on the repository.
data Line
  = BranchLine RepoId Branch
  | PullRequestLine RepoId GhPullRequestId
  deriving stock (Eq, Ord, Show)

-- | The runs this server process has in flight, by line.
data Supersession = Supersession
  { -- | Off when the operator turned cancelling off for the whole server.
    supersessionEnabled :: Bool,
    supersessionLines :: TVar (Map Line [LineRun]),
    supersessionNextRunId :: TVar Int,
    -- | The last closure computed of the derivations live runs need. Shared,
    -- since every superseded build that is still running asks for it.
    supersessionNeededClosure :: MVar NeededClosure
  }

-- | One run of 'Garnix.Build.Flake.runBuildFlake' for a commit on a line.
data LineRun = LineRun
  { lineRunId :: Int,
    lineRunLine :: Line,
    lineRunCommit :: CommitHash,
    -- | The commit that superseded this run, once one has.
    lineRunSupersededBy :: TVar (Maybe CommitHash),
    -- | Whether the run gives up its work, decided by its @cancelSuperseded@
    -- once it is superseded. Everything that cancels waits on this.
    lineRunCancelling :: TVar (Maybe Bool),
    -- | How many of the run's builds have not got their drvPath yet.
    -- 'Nothing' until the run knows its builds.
    lineRunUnevaluated :: TVar (Maybe Int),
    -- | The drvPaths the run's builds evaluated to so far.
    lineRunDrvs :: TVar (Set Nix.DrvPath)
  }

-- | Where one build of a run is.
data BuildProgress = BuildProgress
  { -- | Whether the build is done evaluating, and so counted in
    -- 'lineRunUnevaluated'.
    buildProgressEvaluated :: TVar Bool,
    -- | 'Nothing' until its @nix build@ holds a build slot. Then the
    -- derivations it is building at the moment, by nix's activity id.
    buildProgressBuilding :: TVar (Maybe (Map InternalId Text))
  }

data NeededClosure = NeededClosure
  { neededFor :: Set Nix.DrvPath,
    neededPaths :: Set Text
  }
