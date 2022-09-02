{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Holiplan.Plan (
  PlanDetail (..),
  PlanIndex (..),
  ReqPlan (..),
  Plan (..),
  Error (..),
  PlanId (..),
  listPlans,
  createPlan,
  getPlanDetail,
) where

import qualified DB
import Data.Aeson (Result (Error, Success), ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (Options (fieldLabelModifier), defaultOptions, deriveJSON)
import Data.Aeson.Types (FromJSON)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hasql.Pool (Pool, UsageError)
import qualified Hasql.Pool as Pool
import Hasql.TH (singletonStatement)
import Holiplan.Session (CurrentUserId (CurrentUserId))

newtype PlanId = PlanId UUID
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via UUID

data PlanDetail = PlanDetail
  { plan_detail_id :: PlanId,
    plan_detail_date :: Day,
    plan_detail_name :: Text,
    plan_detail_description :: Text,
    plan_detail_events :: [Text],
    plan_detail_comments :: [Text]
  }
  deriving stock (Eq, Show, Generic)

data Plan = Plan
  { plan_plan_id :: PlanId,
    plan_date :: Day,
    plan_name :: Text,
    plan_description :: Text
  }
  deriving stock (Eq, Show, Generic)

data ReqPlan = ReqPlan
  { req_plan_name :: Text,
    req_plan_description :: Text,
    req_plan_date :: Day
  }
  deriving stock (Eq, Show, Generic)

data PlanIndex = PlanIndex
  { plan_index_data :: [Plan],
    plan_index_length :: Int
  }
  deriving stock (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 12} ''PlanDetail)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Plan)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 9} ''ReqPlan)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 11} ''PlanIndex)

data Error
  = UsageError UsageError
  | ParseError String
  deriving stock (Show)

listPlans :: Pool -> CurrentUserId -> IO (Either Error PlanIndex)
listPlans dbPool currentUserId =
  let statement =
        [singletonStatement|
            SELECT list_plans :: JSONB
              FROM api.list_plans()
          |]

      result =
        Pool.use dbPool $
          DB.authQuery
            (coerce @CurrentUserId @Int64 currentUserId)
            ()
            statement

      planIndex =
        result >>= \case
          Right resultValue ->
            case Aeson.fromJSON @PlanIndex resultValue of
              Success planIndex' -> pure (Right planIndex')
              Error e -> pure (Left $ ParseError e)
          Left e -> pure (Left $ UsageError e)
   in planIndex

createPlan :: Pool -> CurrentUserId -> ReqPlan -> IO (Either Error Plan)
createPlan dbPool currentUserId (ReqPlan name desc date) =
  let statement =
        [singletonStatement|
          SELECT create_plan :: JSONB
            FROM api.create_plan(
              $1 :: TEXT,
              $2 :: TEXT,
              $3 :: DATE,
              $4 :: TEXT,
              $5 :: TEXT
            )
        |]

      result =
        Pool.use dbPool $
          DB.authQuery
            (coerce @CurrentUserId @Int64 currentUserId)
            (name, desc, date, "REPLACE_ME", "PH")
            statement

      plan =
        result >>= \case
          Right resultValue ->
            case Aeson.fromJSON @Plan resultValue of
              Success planDetail' -> pure (Right planDetail')
              Error e -> pure (Left $ ParseError e)
          Left e -> pure (Left $ UsageError e)
   in plan

getPlanDetail :: Pool -> CurrentUserId -> PlanId -> IO (Either Error PlanDetail)
getPlanDetail dbPool currentUserId planId =
  let statement =
        [singletonStatement|
          SELECT get_plan_details :: JSONB
            FROM api.get_plan_details($1 :: UUID)
        |]

      result =
        Pool.use dbPool $
          DB.authQuery
            (coerce @CurrentUserId @Int64 currentUserId)
            (coerce @PlanId @UUID planId)
            statement

      planDetail =
        result >>= \case
          Right resultValue ->
            case Aeson.fromJSON @PlanDetail resultValue of
              Success planDetail' -> pure (Right planDetail')
              Error e -> pure (Left $ ParseError e)
          Left e -> pure (Left $ UsageError e)
   in planDetail
