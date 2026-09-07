module Garnix.Nix.Plan
  ( getPlanOf,
  )
where

import Data.Attoparsec.Text.Lazy hiding (take)
import Data.ByteString (ByteString)
import Data.Char
import Data.Text qualified as T
import Data.Text.Lazy.IO (readFile)
import Data.Tuple.Extra
import Garnix.Monad
import Garnix.Nix.Types
import Garnix.Prelude hiding (readFile)
import Garnix.Types hiding (head)
import Nix.Derivation qualified

mapToStorePaths :: (ConvertibleStrings a Text) => [a] -> M [StorePath]
mapToStorePaths paths = forM paths $ either (throw . OtherError) pure . parseStorePath

mapToDrvPaths :: [Text] -> M [DrvPath]
mapToDrvPaths paths = map DrvPath <$> mapToStorePaths paths

getDrvOutputPaths :: DrvPath -> M [StorePath]
getDrvOutputPaths drvFile = do
  drvText <- liftIO $ readFile $ cs drvFile
  case parse Nix.Derivation.parseDerivation drvText of
    Fail _ _ err -> throw $ FailedToParseDrvFile (cs drvFile) $ cs err
    Done _ parsed -> mapToStorePaths $ map Nix.Derivation.path $ toList $ Nix.Derivation.outputs parsed

getPlanOf :: ByteString -> M Plan
getPlanOf = mockable #getBuildPlanMock $ \input -> do
  (toBuild, remaining) <-
    firstM mapToDrvPaths
      . span ("/nix/store/" `T.isPrefixOf`)
      . fmap (T.dropWhile isSpace)
      . drop 1
      . dropWhile (not . isStartOfDerivationPaths)
      . T.lines
      . cs
      $ input
  case remaining of
    firstRemaining : _
      | not ("will be fetched" `T.isInfixOf` firstRemaining) ->
          log Critical "Expected `nix build --dry-run` to output a list of derivations to be built followed by optionally a list of derivations to be fetched"
    _ -> pure ()
  outputHashes <- forM toBuild $ \drvPath -> (drvPath,) <$> getDrvOutputPaths drvPath
  pure $ Plan outputHashes

isStartOfDerivationPaths :: Text -> Bool
isStartOfDerivationPaths line
  | "derivations will be built:" `T.isSuffixOf` line = True
  | "derivation will be built:" `T.isSuffixOf` line = True
  | otherwise = False
