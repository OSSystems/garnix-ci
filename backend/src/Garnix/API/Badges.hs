{-# LANGUAGE TemplateHaskell #-}

module Garnix.API.Badges (Badge (..), badgesAPI) where

import Data.Aeson qualified as Aeson
import Data.FileEmbed
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Prelude
import Garnix.Types

badgesAPI :: RepoId -> Maybe Branch -> M Badge
badgesAPI repoId' mGivenBranch = do
  let branchSummary :: Branch -> M Badge
      branchSummary branch = buildSummary <$> DB.getLatestBuildsForBranch repoId' branch
  case mGivenBranch of
    Just givenBranch -> branchSummary givenBranch
    Nothing -> do
      eitherRepo <- getDefaultBranch Nothing repoId'
      case eitherRepo of
        Nothing -> pure $ Badge "build status unknown"
        Just defaultBranch -> branchSummary defaultBranch

buildSummary :: [Build] -> Badge
buildSummary builds =
  let successes = length $ filter ((== Just Success) . view status) builds
      failures = length $ filter ((`elem` [Just Failure, Just Cancelled]) . view status) builds
      inProgress = length $ filter ((== Nothing) . view status) builds
   in Badge $ case (successes, failures, inProgress) of
        (0, 0, 0) -> "build status unknown"
        (0, 0, _) -> "build in progress"
        (1, 0, 0) -> "1 build succeeded"
        (1, 0, 1) -> "1 build succeeded, 1 build in progress"
        (1, 0, pending) -> "1 build succeeded, " <> show pending <> " builds in progress"
        (success, 0, 0) -> show success <> " builds succeeded"
        (success, 0, 1) -> show success <> " builds succeeded, 1 build in progress"
        (success, 0, pending) -> show success <> " builds succeeded, " <> show pending <> " builds in progress"
        (0, _failure, 0) -> "all builds failed"
        (1, failure, 0) -> "1 build succeeded out of " <> show (1 + failure)
        (success, failure, 0) -> show success <> " builds succeeded out of " <> show (success + failure)
        (success, failure, pending) -> show success <> " succeeded, " <> show failure <> " failed, and " <> show pending <> " in progress"

data Badge = Badge {badgeMessage :: Text}
  deriving stock (Show)

instance ToJSON Badge where
  toJSON :: Badge -> Aeson.Value
  toJSON Badge {..} =
    Aeson.object
      [ "label" Aeson..= (" " :: Text),
        "labelColor" Aeson..= ("white" :: Text),
        "color" Aeson..= ("black" :: Text),
        "message" Aeson..= badgeMessage,
        "logoSvg" Aeson..= faviconSvg
      ]
    where
      faviconSvg :: String
      faviconSvg =
        $( do
             badgeFile <- makeRelativeToProject "data/badge_favicon.svg"
             embedStringFile badgeFile
         )
