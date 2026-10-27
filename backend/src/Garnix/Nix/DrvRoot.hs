module Garnix.Nix.DrvRoot
  ( withDrvRoot,
    addDrvRoot,
    removeStaleDrvRoots,
  )
where

import Cradle
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Monad.SubProcess (runSubProcess_)
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Types
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removePathForcibly)

newtype DrvRoot = DrvRoot FilePath

withDrvRoot :: BuildId -> (DrvRoot -> M a) -> M a
withDrvRoot buildId action = do
  dir <- view #drvRootsDir
  let root = dir </> cs (toUrlPiece buildId)
  action (DrvRoot root) `finally` liftIO (removePathForcibly root)

addDrvRoot :: DrvRoot -> Nix.DrvPath -> M ()
addDrvRoot (DrvRoot root) drvPath = do
  nixConfig <- view #userNixConfig
  result <- tryEither $ do
    liftIO $ createDirectoryIfMissing True (takeDirectory root)
    runSubProcess_
      $ cmd "nix"
      & addArgs ["build", cs drvPath, "--out-link", cs root :: Text]
      & addNixConfigEnvironment nixConfig
      & silenceStdout
  case result of
    Right () -> pure ()
    Left problem -> log Error $ "addDrvRoot: could not root " <> cs drvPath <> ": " <> show problem

removeStaleDrvRoots :: M ()
removeStaleDrvRoots = do
  dir <- view #drvRootsDir
  exists <- liftIO $ doesDirectoryExist dir
  when exists $ do
    names <- liftIO $ listDirectory dir
    stale <- flip filterM names $ \name -> case parseUrlPiece @BuildId (cs name) of
      Left _ -> pure True
      Right buildId -> not <$> DB.isBuildPending buildId
    unless (null stale) $ do
      log Notice $ "removeStaleDrvRoots: " <> show (length stale) <> " root(s) of builds no longer pending"
      forM_ stale $ \name -> liftIO $ removePathForcibly (dir </> name)
