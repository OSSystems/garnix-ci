module Garnix.TestHelpers.NixStore (garbageCollectStorePath) where

import Control.Concurrent qualified
import Control.Monad
import Data.Char (isSpace)
import Garnix.Prelude
import System.Exit (ExitCode (..))
import System.IO
import System.Process (readProcessWithExitCode)

garbageCollectStorePath :: FilePath -> IO ()
garbageCollectStorePath path = do
  assertNoGcRoots
  referrers <- lines <$> runProcess "nix-store" ["--query", "--referrers", path]
  forM_ referrers garbageCollectStorePath
  when (".drv" `isSuffixOf` path) $ do
    outputs <- lines <$> runProcess "nix-store" ["--query", "--outputs", path]
    forM_ outputs garbageCollectStorePath
  deriverPath <- getDeriver path
  void $ runProcess "nix-store" $ ["--delete", path] ++ maybe [] pure deriverPath
  where
    assertNoGcRoots =
      let go (n :: Int) = do
            gcRoots <- lines <$> runProcess "nix-store" ["--query", "--roots", path]
            case gcRoots of
              [] -> pure ()
              _ : _ -> do
                let procGcRoots = filter ("/proc" `isPrefixOf`) gcRoots
                if procGcRoots == gcRoots && n > 0
                  then do
                    hPutStrLn System.IO.stderr "found gc roots in /proc, waiting for them to disappear..."
                    Control.Concurrent.threadDelay 50000
                    go (n - 1)
                  else error $ "garbageCollectStorePath: cannot remove path because of gc roots: " <> show gcRoots
       in go 1000

    getDeriver :: String -> IO (Maybe FilePath)
    getDeriver path = do
      maybePath <- runProcessMaybe "nix-store" ["--query", "--deriver", path]
      return $ maybePath >>= checkPath
      where
        checkPath path
          | "unknown-deriver" `isInfixOf` path || all isSpace path = Nothing
          | otherwise = Just . dropWhileEnd isSpace $ path

    runProcessMaybe :: String -> [String] -> IO (Maybe String)
    runProcessMaybe command args = do
      (exitCode, stdout, _) <- readProcessWithExitCode command args ""
      case exitCode of
        ExitSuccess -> pure $ Just stdout
        ExitFailure _ -> pure Nothing

    runProcess :: String -> [String] -> IO String
    runProcess command args = do
      (exitCode, stdout, stderr) <- readProcessWithExitCode command args ""
      case exitCode of
        ExitSuccess -> pure stdout
        ExitFailure _ -> do
          hPutStrLn System.IO.stderr stderr
          hPutStrLn System.IO.stderr stdout
          error . cs $ "command failed: " <> unwords (command : args)
