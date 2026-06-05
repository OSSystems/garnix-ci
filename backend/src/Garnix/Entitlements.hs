module Garnix.Entitlements
  ( addDefaultEntitlements,
    addProduct,
    queryCiTimeEntitlements,
    hasRemainingCiTime,

    -- * products
    ProductToken (..),
    PriceId (..),
    displayName,
    title,
    baseCiTime,
    defaultPlanName,
    getPlan,
    getPlans,
    getPlanByProductToken,
    getPlanByName,

    -- * usage limits
    emptyUsageLimits,
    setExtraUsageLimits,
  )
where

import Data.Map.Strict (Map, alter)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.PostgreSQL.Typed (pgSQL)
import Garnix.DB qualified as DB
import Garnix.Duration
import Garnix.Monad (M, throw)
import Garnix.MonetaryCost
import Garnix.Prelude
import Garnix.Types

newtype PriceId = PriceId {getPriceId :: Text}
  deriving newtype (Eq, Show)

defaultPlanName :: Text
defaultPlanName = "free-v1"

fallbackPackagesPerFlake :: Int32
fallbackPackagesPerFlake = 100

fallbackEvaluationTimeout :: Int16
fallbackEvaluationTimeout = 30

fallbackBuildTimeout :: Int16
fallbackBuildTimeout = 120

addDefaultEntitlements :: GhRepoOwner -> M ()
addDefaultEntitlements owner = addProduct owner defaultPlanName

addProduct :: GhRepoOwner -> Text -> M ()
addProduct owner product = do
  void
    $ DB.pgExec
      [pgSQL|
        INSERT INTO repo_owner_has_product
          ( repo_owner, product )
        VALUES
          ( ${owner}, ${product} )
        ON CONFLICT DO NOTHING
      |]

queryCiTimeEntitlements :: GhRepoOwner -> M Duration
queryCiTimeEntitlements repoOwner = do
  extraCiMinutesResult :: [Int32] <-
    DB.pgQuery
      [pgSQL|
        SELECT extra_ci_time_in_minutes
        FROM repo_owner_usage_limits
        WHERE repo_owner = ${repoOwner}
      |]
  extraCiTime <- case extraCiMinutesResult of
    [minutes] -> pure $ fromMinutes minutes
    [] -> pure emptyDuration
    _ -> throw $ OtherError "impossible: more than one extra ci minutes for repo owner"
  hosts :: [Maybe Int64] <-
    DB.pgQuery
      [pgSQL|
        SELECT max(ci_minutes)
        FROM repo_owner_has_product
        INNER JOIN products
        ON repo_owner_has_product.product = products.name
        WHERE repo_owner_has_product.repo_owner = ${repoOwner}
      |]
  planMinutes <- case hosts of
    [Just result] -> pure $ fromMinutes result
    [Nothing] -> pure emptyDuration
    [] -> pure emptyDuration
    _ : _ : _ -> throw $ OtherError "impossible: more than one result from aggregate query"
  pure $ planMinutes `addDuration` extraCiTime

hasRemainingCiTime :: GhRepoOwner -> M Bool
hasRemainingCiTime owner = do
  total <- queryCiTimeEntitlements owner
  used <- DB.getCurrentMonthUsage owner
  pure $ total >= used

-- * products

newtype ProductToken = ProductToken {getProductToken :: Text}
  deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData)

mergePlans :: [(ProductPlan, Maybe PriceId)] -> Maybe ProductPlan
mergePlans allPlans = case filter (isJust . snd) allPlans of
  [] -> merge $ fmap fst allPlans
  withPriceIds -> do
    allMerged <- merge $ fmap fst allPlans
    withPriceIdsMerged <- merge $ fmap fst withPriceIds
    Just
      $ ProductPlan
        { _productPlanDisplayName = withPriceIdsMerged ^. displayName,
          _productPlanDescription = withPriceIdsMerged ^. description,
          _productPlanBaseCiTime = allMerged ^. baseCiTime,
          _productPlanMaximumPrDeploymentTime = allMerged ^. maximumPrDeploymentTime,
          _productPlanIncludedBranchDeploymentHosts = allMerged ^. includedBranchDeploymentHosts,
          _productPlanMaximumPackagesPerFlake = allMerged ^. maximumPackagesPerFlake,
          _productPlanPackageEvaluationTimeout = allMerged ^. packageEvaluationTimeout,
          _productPlanPackageBuildTimeout = allMerged ^. packageBuildTimeout,
          _productPlanExtraUsage = allMerged ^. extraUsage,
          _productPlanIsPaid = allMerged ^. isPaid
        }
  where
    merge :: [ProductPlan] -> Maybe ProductPlan
    merge plans = case sortBy (compare `on` _productPlanDisplayName) plans of
      [] -> Nothing
      a : as -> Just $ foldl' merge a as
        where
          merge a b =
            ProductPlan
              { _productPlanDisplayName = a ^. displayName <> ", " <> b ^. displayName,
                _productPlanDescription = case catMaybes [a ^. description, b ^. description] of
                  [] -> Nothing
                  snippets -> Just $ T.intercalate ", " snippets,
                _productPlanBaseCiTime = maxDuration (a ^. baseCiTime) (b ^. baseCiTime),
                _productPlanMaximumPrDeploymentTime = maxDuration (a ^. maximumPrDeploymentTime) (b ^. maximumPrDeploymentTime),
                _productPlanIncludedBranchDeploymentHosts = max (a ^. includedBranchDeploymentHosts) (b ^. includedBranchDeploymentHosts),
                _productPlanMaximumPackagesPerFlake = max (a ^. maximumPackagesPerFlake) (b ^. maximumPackagesPerFlake),
                _productPlanPackageEvaluationTimeout = max (a ^. packageEvaluationTimeout) (b ^. packageEvaluationTimeout),
                _productPlanPackageBuildTimeout = max (a ^. packageBuildTimeout) (b ^. packageBuildTimeout),
                _productPlanExtraUsage =
                  ExtraUsageLimits
                    { ciTime = maxDuration (a ^. extraUsage . #ciTime) (b ^. extraUsage . #ciTime),
                      prDeployTime = maxDuration (a ^. extraUsage . #prDeployTime) (b ^. extraUsage . #prDeployTime),
                      hostingSpend = max (a ^. extraUsage . #hostingSpend) (b ^. extraUsage . #hostingSpend)
                    },
                _productPlanIsPaid = (a ^. isPaid) || (b ^. isPaid)
              }

getPlan :: GhRepoOwner -> M ProductPlan
getPlan repoOwner =
  Map.lookup repoOwner <$> getPlans [repoOwner] >>= \case
    Nothing -> throw $ OtherError $ "Impossible: no plan found for " <> getGhLogin (getGhRepoOwner repoOwner)
    Just plan -> pure plan

getPlans :: [GhRepoOwner] -> M (Map GhRepoOwner ProductPlan)
getPlans orgs = do
  forM_ orgs addDefaultEntitlements
  res ::
    [ ( GhRepoOwner,
        Text,
        Maybe Text,
        Maybe Text,
        Maybe Int64,
        Maybe Int64,
        Maybe Int64,
        Maybe Text,
        Maybe Int32,
        Maybe Int16,
        Maybe Int16,
        Maybe Int32,
        Maybe Int32,
        Maybe Int32
      )
    ] <-
    DB.pgQuery
      [pgSQL|
        SELECT
          repo_owner_has_product.repo_owner,
          name,
          title,
          description,
          hosting,
          pr_hosting,
          ci_minutes,
          price_id,
          packages_per_flake,
          package_eval_timeout_in_minutes,
          package_build_timeout_in_minutes,
          extra_ci_time_in_minutes,
          extra_pr_hosting_in_minutes,
          extra_hosting_spending_limit_in_usd
        FROM repo_owner_has_product
        INNER JOIN products
        ON repo_owner_has_product.product = products.name
        LEFT JOIN repo_owner_usage_limits
        on repo_owner_usage_limits.repo_owner = repo_owner_has_product.repo_owner
        WHERE repo_owner_has_product.repo_owner = ANY(${orgs})
      |]
  pure
    $ Map.mapMaybe mergePlans
    $ foldl'
      ( \acc
         ( repoOwner,
           name,
           title,
           description,
           branchHosting,
           prDeployMinutes,
           ciMinutes,
           priceId,
           packagesPerFlake,
           packageEvalTimeout,
           packageBuildTimeout,
           extraCiMinutes,
           extraPrMinutes,
           extraHostingSpendInUsd
           ) ->
            Data.Map.Strict.alter
              ( Just
                  . ( ( ProductPlan
                          { _productPlanDisplayName = fromMaybe name title,
                            _productPlanDescription = description,
                            _productPlanBaseCiTime = maybe emptyDuration fromMinutes ciMinutes,
                            _productPlanMaximumPrDeploymentTime = maybe emptyDuration fromMinutes prDeployMinutes,
                            _productPlanIncludedBranchDeploymentHosts = fromMaybe 0 branchHosting,
                            _productPlanMaximumPackagesPerFlake = fromMaybe fallbackPackagesPerFlake packagesPerFlake,
                            _productPlanPackageEvaluationTimeout = fromMaybe fallbackEvaluationTimeout packageEvalTimeout,
                            _productPlanPackageBuildTimeout = fromMaybe fallbackBuildTimeout packageBuildTimeout,
                            _productPlanExtraUsage =
                              ExtraUsageLimits
                                { ciTime = maybe emptyDuration fromMinutes extraCiMinutes,
                                  prDeployTime = maybe emptyDuration fromMinutes extraPrMinutes,
                                  hostingSpend = usd $ fromMaybe 0 extraHostingSpendInUsd
                                },
                            _productPlanIsPaid = isJust priceId
                          },
                        fmap PriceId priceId
                      )
                        :
                    )
                  . fromMaybe []
              )
              repoOwner
              acc
      )
      mempty
      res

getPlanByProductToken :: ProductToken -> M (Maybe (Text, Maybe PriceId, ProductPlan))
getPlanByProductToken productToken = do
  res <-
    fmap
      ( \( name :: Text,
           title :: Maybe Text,
           description :: Maybe Text,
           priceId :: Maybe Text,
           branchHosting :: Maybe Int64,
           prDeployMinutes :: Maybe Int64,
           ciMinutes :: Maybe Int64,
           packagesPerFlake :: Maybe Int32,
           packageEvalTimeout :: Maybe Int16,
           packageBuildTimeout :: Maybe Int16
           ) ->
            Just
              ( name,
                fmap PriceId priceId,
                ProductPlan
                  { _productPlanDisplayName = fromMaybe name title,
                    _productPlanDescription = description,
                    _productPlanBaseCiTime = maybe emptyDuration fromMinutes ciMinutes,
                    _productPlanMaximumPrDeploymentTime = maybe emptyDuration fromMinutes prDeployMinutes,
                    _productPlanIncludedBranchDeploymentHosts = fromMaybe 0 branchHosting,
                    _productPlanMaximumPackagesPerFlake = fromMaybe fallbackPackagesPerFlake packagesPerFlake,
                    _productPlanPackageEvaluationTimeout = fromMaybe fallbackEvaluationTimeout packageEvalTimeout,
                    _productPlanPackageBuildTimeout = fromMaybe fallbackBuildTimeout packageBuildTimeout,
                    _productPlanExtraUsage = emptyUsageLimits,
                    _productPlanIsPaid = isJust priceId
                  }
              )
      )
      <$> DB.pgQuery
        [pgSQL|
          SELECT
            name,
            title,
            description,
            price_id,
            hosting,
            pr_hosting,
            ci_minutes,
            packages_per_flake,
            package_eval_timeout_in_minutes,
            package_build_timeout_in_minutes
          FROM products
          WHERE token = ${getProductToken productToken}
        |]
  case res of
    [p] -> pure p
    [] -> pure Nothing
    _ -> throw $ OtherError "Impossible: price_id is unique"

getPlanByName :: Text -> M (Maybe ProductPlan)
getPlanByName planName = do
  res <-
    DB.pgQuery
      [pgSQL|
        SELECT
          name,
          description,
          ci_minutes,
          pr_hosting,
          hosting,
          packages_per_flake,
          package_eval_timeout_in_minutes,
          package_build_timeout_in_minutes,
          price_id
        FROM products
        WHERE name = ${planName}
      |]
  let plans =
        map
          ( \( name :: Text,
               description :: Maybe Text,
               ciMinutes :: Maybe Int64,
               prDeployMinutes :: Maybe Int64,
               hosting :: Maybe Int64,
               packagesPerFlake :: Maybe Int32,
               packageEvalTimeoutInMinutes :: Maybe Int16,
               packageBuildTimeoutInMinutes :: Maybe Int16,
               priceId :: Maybe Text
               ) ->
                ProductPlan
                  { _productPlanDisplayName = name,
                    _productPlanDescription = description,
                    _productPlanBaseCiTime = maybe emptyDuration fromMinutes ciMinutes,
                    _productPlanMaximumPrDeploymentTime = maybe emptyDuration fromMinutes prDeployMinutes,
                    _productPlanIncludedBranchDeploymentHosts = fromMaybe 0 hosting,
                    _productPlanMaximumPackagesPerFlake = fromMaybe fallbackPackagesPerFlake packagesPerFlake,
                    _productPlanPackageEvaluationTimeout = fromMaybe fallbackEvaluationTimeout packageEvalTimeoutInMinutes,
                    _productPlanPackageBuildTimeout = fromMaybe fallbackBuildTimeout packageBuildTimeoutInMinutes,
                    _productPlanExtraUsage = emptyUsageLimits,
                    _productPlanIsPaid = isJust priceId
                  }
          )
          res
  case plans of
    [plan] -> pure (Just plan)
    [] -> pure Nothing
    _ -> throw $ OtherError $ "Impossible: more than one plan with name " <> planName

setExtraUsageLimits :: GhRepoOwner -> ExtraUsageLimits -> M ()
setExtraUsageLimits owner newLimits = do
  let extraCiMinutes :: Int32 = floor $ toMinutes $ newLimits ^. #ciTime
  let extraPrMinutes :: Int32 = floor $ toMinutes $ newLimits ^. #prDeployTime
  let extraHostingSpendInUsd :: Int32 = getCostInCents (newLimits ^. #hostingSpend) `quot` 100
  void
    $ DB.pgExec
      [pgSQL|
        INSERT INTO repo_owner_usage_limits
          ( repo_owner, extra_ci_time_in_minutes, extra_pr_hosting_in_minutes, extra_hosting_spending_limit_in_usd )
        VALUES
          ( ${owner}, ${extraCiMinutes}, ${extraPrMinutes}, ${extraHostingSpendInUsd} )
        ON CONFLICT (repo_owner) DO UPDATE
          SET
            extra_ci_time_in_minutes = ${extraCiMinutes},
            extra_pr_hosting_in_minutes = ${extraPrMinutes},
            extra_hosting_spending_limit_in_usd = ${extraHostingSpendInUsd}
      |]

