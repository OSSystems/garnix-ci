module Garnix.Incremental
  ( makeNormalizedFlake,
    renderNormalizedFlakeWithHelpers,
    withIntermediatesFlake,
    incrementalCacheUrl,
    NormalizedFlake (..),
  )
where

import Control.Lens (At, Index, IxValue, Ixed)
import Cradle
import Data.Map qualified as Map
import Data.Monoid
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Nix.StorePath
import Garnix.Nix.Types qualified as Nix
import Garnix.Prelude
import Garnix.Types
import System.IO.Temp

withIntermediatesFlake :: Build -> (Maybe FilePath -> M a) -> M a
withIntermediatesFlake build action = do
  builds <-
    (previousCandidates >>= DB.getIncrementalTarget build)
      <?> "Getting incremental build base candidates"
  case builds of
    [] -> do
      log Informational "No builds found for incrementalization"
      action Nothing
    _ -> do
      emptyDir' <- view #emptyDir
      cacheUrl <- incrementalCacheUrl
      flakeContents <-
        liftIO
          . renderNormalizedFlakeWithHelpers emptyDir' cacheUrl
          =<< makeNormalizedFlake builds
      log Informational $ "Using the following flake file for garnix-incrementalize:\n "
        <> flakeContents
      withSystemTempDirectory "incremental-build" $ \fp -> do
        liftIO $ T.writeFile (fp <> "/flake.nix") flakeContents
        action (Just fp)

-- | The garnix instance is its own binary cache; see "Garnix.API.Cache".
incrementalCacheUrl :: M Text
incrementalCacheUrl = do
  base <- view #baseUrl
  pure $ T.dropWhileEnd (== '/') base <> "/api/cache"

previousCandidates :: M [CommitHash]
previousCandidates = do
  workingDir <- view #workingDir
  (StdoutTrimmed out, e) <-
    run
      $ cmd "git"
      & addArgs ["rev-list", "-n", "5", "HEAD" :: String]
      & setWorkingDir workingDir
  case e of
    ExitSuccess -> pure $ CommitHash <$> tail (T.lines out)
    _ -> throw $ OtherError "Could not get rev-list for incrementalization"

makeNormalizedFlake :: [Build] -> M NormalizedFlake
makeNormalizedFlake = foldM go mempty
  where
    go :: NormalizedFlake -> Build -> M NormalizedFlake
    go f build
      | build ^. packageType == TypeOverall = pure f
      | otherwise = do
          withStorePath build "intermediates" $ \storePath -> do
            pure
              $ f
              & at
                ( build ^. packageType,
                  build ^. system,
                  build ^. package
                )
                .~ storePath

renderNormalizedFlakeWithHelpers :: FilePath -> Text -> NormalizedFlake -> IO Text
renderNormalizedFlakeWithHelpers emptyDir' cacheUrl (NormalizedFlake f) = cs <$> rendered
  where
    renderSingle :: (PackageType, MaybeSystem, PackageName) -> Nix.StorePath -> Text -> Text
    renderSingle (typ, sys, PackageName name) s prev =
      prev
        <> "\n"
        <> (typ ^. re asPackageType)
        <> "s."
        <> case sys of
          NoSystem -> ""
          IsSystem s -> s ^. systemTextIso <> "."
        <> name
        <> case typ of
          TypeNixosConfiguration -> ".config.system.build.toplevel"
          _ -> ""
        <> ".intermediates"
        <> " = builtins.fetchClosure { "
        <> "     inputAddressed = true;" -- Is this worth changing?
        <> "     fromStore = \""
        <> cacheUrl
        <> "\"; "
        <> "     fromPath = \""
        <> cs s
        <> "\";"
        <> " };"
    attrs :: Text
    attrs = Map.foldrWithKey renderSingle "" f

    helpers = do
      pure
        [i|
        lib.withCaches = args.self.lib.withCachesFor args.self;

        lib.withCachesFor = prev: outputs:
         let wantedAttrs = ["packages" "checks" "devShells"];
             emptyDir = "${#{emptyDir'}}";
             mapAttrsIfSet = fn : s : if builtins.isAttrs s then builtins.mapAttrs fn s else s;
          in (mapAttrsIfSet (type:
               mapAttrsIfSet (sys:
                 mapAttrsIfSet (pkg: def:
                   if builtins.elem type wantedAttrs && builtins.isFunction def
                   then (def (prev.${type}.${sys}.${pkg}.intermediates or emptyDir))
                   else def
                 )))) outputs;

        |]
    rendered = do
      h <- helpers
      pure
        [i|
      {
        outputs = args : {
          #{attrs}
          #{h}
        };
      }
    |]

-- * Types

-- | Represents a Flake that has no eval left to be done, and without any inputs.
-- This can be thought of as the "normal form" of a flake.
newtype NormalizedFlake
  = NormalizedFlake
      (Map.Map (PackageType, MaybeSystem, PackageName) Nix.StorePath)
  deriving newtype (Semigroup, Monoid)

instance At NormalizedFlake where
  at i = co . at i . coerced
    where
      co ::
        Iso'
          NormalizedFlake
          (Map.Map (PackageType, MaybeSystem, PackageName) Nix.StorePath)
      co = coerced

instance Ixed NormalizedFlake

type instance Index NormalizedFlake = (PackageType, MaybeSystem, PackageName)

type instance IxValue NormalizedFlake = Nix.StorePath

newtype Output = Output Text
  deriving newtype (Eq, Ord, IsString)
