-- | Parsing and host-relative resolution of the hosting resource budgets.
module Garnix.Hosting.Budget
  ( BudgetSpec (..),
    parseBudget,
    renderBudget,
    resolveBudget,
    hostTotalMiB,
    hostVcpus,
  )
where

import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import GHC.Conc (getNumProcessors)
import Garnix.Prelude
import Text.Read (readMaybe)

-- | A budget as configured: an absolute total, or an amount to keep free
-- on the host. Units are MiB for memory, whole cores for cpu.
data BudgetSpec = Absolute Int | Reserve Int
  deriving stock (Eq, Show)

-- | Parse the env encoding @total:\<n\>@ / @reserve:\<n\>@.
parseBudget :: Text -> Maybe BudgetSpec
parseBudget spec = case T.splitOn ":" (T.strip spec) of
  ["total", n] -> Absolute <$> readMaybe (cs n)
  ["reserve", n] -> Reserve <$> readMaybe (cs n)
  _ -> Nothing

-- | Inverse of 'parseBudget'.
renderBudget :: BudgetSpec -> Text
renderBudget = \case
  Absolute n -> "total:" <> cs (show n)
  Reserve n -> "reserve:" <> cs (show n)

-- | Resolve to an absolute cap given the host total for that dimension.
resolveBudget :: Int -> Maybe BudgetSpec -> Maybe Int
resolveBudget _ Nothing = Nothing
resolveBudget _ (Just (Absolute absolute)) = Just absolute
resolveBudget hostTotal (Just (Reserve reserve)) = Just (max 0 (hostTotal - reserve))

-- | Host RAM in MiB, from @\/proc\/meminfo@'s @MemTotal@ (reported in kB).
-- 'Nothing' when the field is absent or unparseable.
hostTotalMiB :: IO (Maybe Int)
hostTotalMiB = do
  contents <- readFile "/proc/meminfo"
  pure
    $ (`div` 1024)
    <$> listToMaybe
      [ n
        | line <- lines contents,
          ["MemTotal:", value, "kB"] <- [words line],
          Just n <- [readMaybe value]
      ]

-- | Number of host CPUs, as the RTS sees the machine.
hostVcpus :: IO Int
hostVcpus = getNumProcessors
