{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Holiplan.Plan (
  PlanDetail (..),
  PlanIndex (..),
  ReqPlan (..),
  Plan (..),
  ListError (..),
  listPlans,
) where

import qualified DB
import Data.Aeson (Result (Error, Success))
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (Options (fieldLabelModifier), defaultOptions, deriveJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Hasql.Pool (Pool, UsageError)
import qualified Hasql.Pool as Pool
import Hasql.TH (singletonStatement)

data PlanDetail = PlanDetail
  { plan_detail_id :: Int
  , plan_detail_date :: Day
  , plan_detail_name :: Text
  , plan_detail_description :: Text
  , plan_detail_events :: [Text]
  , plan_detail_comments :: [Text]
  }
  deriving stock (Eq, Show, Generic)

data Plan = Plan
  { plan_id :: Int
  , plan_date :: Day
  , plan_name :: Text
  , plan_description :: Text
  }
  deriving stock (Eq, Show, Generic)

data ReqPlan = ReqPlan
  { req_plan_name :: Text
  , req_plan_description :: Text
  , req_plan_date :: Day
  }
  deriving stock (Eq, Show, Generic)

data PlanIndex = PlanIndex
  { plan_index_data :: [PlanDetail]
  , plan_index_length :: Int
  }
  deriving stock (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 13} ''PlanDetail)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Plan)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 9} ''ReqPlan)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 11} ''PlanIndex)

data ListError
  = UsageError UsageError
  | ParseError String

listPlans :: Pool -> Int64 -> IO (Either ListError PlanIndex)
listPlans dbPool userId =
  let statement =
        [singletonStatement|
            SELECT list_plans :: JSONB
              FROM api.list_plans()
          |]

      result = Pool.use dbPool (DB.authQuery userId () statement)

      planIndex =
        result
          >>= \case
            Right resultValue ->
              case Aeson.fromJSON @PlanIndex resultValue of
                Success planIndex' -> pure (Right planIndex')
                Error e -> pure (Left $ ParseError e)
            Left e -> pure (Left $ UsageError e)
   in planIndex
