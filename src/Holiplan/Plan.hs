{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Holiplan.Plan (
  PlanDetail (..),
  PlanIndex (..),
  ReqPlan (..),
  Plan (..),
  ReqComment (..),
  Comment (..),
  CommentId (..),
  Error (..),
  PlanId (..),
  listPlans,
  createPlan,
  getPlanDetail,
  editPlan,
  deletePlan,
  addComment,
  editComment,
  deleteComment,
) where

import qualified DB
import Data.Aeson (Result (Error, Success), ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (Options (fieldLabelModifier), defaultOptions, deriveJSON)
import Data.Aeson.Types (FromJSON)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Hasql.Pool (Pool, UsageError)
import qualified Hasql.Pool as Pool
import Hasql.TH (resultlessStatement, singletonStatement)
import Holiplan.Session (CurrentUserId (CurrentUserId))
import Servant.API (FromHttpApiData)

newtype PlanId = PlanId UUID
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, FromHttpApiData) via UUID

data PlanDetail = PlanDetail
  { plan_detail_id :: PlanId,
    plan_detail_date :: Day,
    plan_detail_name :: Text,
    plan_detail_description :: Text,
    plan_detail_events :: [Text],
    plan_detail_comments :: [Comment]
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

newtype CommentId = CommentId UUID
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, FromHttpApiData) via UUID

data Comment = Comment
  { comment_id :: CommentId,
    user_id :: Int64,
    content :: Text,
    created_at :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

newtype ReqComment = ReqComment
  {req_comment_content :: Text}

instance FromJSON Comment
instance ToJSON Comment

$(deriveJSON defaultOptions {fieldLabelModifier = drop 12} ''PlanDetail)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Plan)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 9} ''ReqPlan)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 11} ''PlanIndex)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 12} ''ReqComment)

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
              Error e -> liftIO (print e) >> pure (Left $ ParseError e)
          Left e -> pure (Left $ UsageError e)
   in planDetail

editPlan :: Pool -> CurrentUserId -> PlanId -> ReqPlan -> IO (Either Error Plan)
editPlan dbPool currentUserId planId (ReqPlan name desc _date) =
  let statement =
        [singletonStatement|
          SELECT edit_plan :: JSONB
            FROM api.edit_plan($1 :: UUID, $2 :: TEXT, $3 :: TEXT)
        |]

      planId' = coerce @PlanId @UUID planId
      currentUserId' = coerce @CurrentUserId @Int64 currentUserId

      result =
        Pool.use dbPool $
          DB.authQuery
            currentUserId'
            (planId', name, desc)
            statement

      plan =
        result >>= \case
          Right resultValue ->
            case Aeson.fromJSON @Plan resultValue of
              Success plan' -> pure (Right plan')
              Error e -> pure (Left $ ParseError e)
          Left e -> pure (Left $ UsageError e)
   in plan

deletePlan :: Pool -> CurrentUserId -> PlanId -> IO (Either UsageError ())
deletePlan dbPool currentUserId planId =
  let statement =
        [resultlessStatement|
          SELECT FROM api.delete_plan($1 :: UUID)
        |]

      planId' = coerce @PlanId @UUID planId

      currentUserId' = coerce @CurrentUserId @Int64 currentUserId
   in Pool.use dbPool $
        DB.authQuery currentUserId' planId' statement

addComment :: Pool -> CurrentUserId -> ReqComment -> PlanId -> IO (Either Error Comment)
addComment dbPool currentUserId (ReqComment comment) planId =
  let statement =
        [singletonStatement|
          SELECT create_comment :: JSONB
            FROM api.create_comment($1 :: UUID, $2 :: TEXT)
        |]

      planId' = coerce @PlanId @UUID planId

      currentUserId' = coerce @CurrentUserId @Int64 currentUserId

      result =
        Pool.use dbPool $
          DB.authQuery
            currentUserId'
            (planId', comment)
            statement

      comment' =
        result >>= \case
          Right resultValue ->
            case Aeson.fromJSON @Comment resultValue of
              Success c -> pure (Right c)
              Error e -> liftIO (print e) >> pure (Left $ ParseError e)
          Left e -> pure (Left $ UsageError e)
   in comment'

editComment :: Pool -> CurrentUserId -> CommentId -> ReqComment -> IO (Either Error Comment)
editComment dbPool currentUserId commentId (ReqComment comment) =
  let statement =
        [singletonStatement|
          SELECT edit_comment :: JSONB
            FROM api.edit_comment($1 :: UUID, $2 :: TEXT)
        |]

      currentUserId' = coerce @CurrentUserId @Int64 currentUserId

      commentId' = coerce @CommentId @UUID commentId

      result =
        Pool.use dbPool $
          DB.authQuery
            currentUserId'
            (commentId', comment)
            statement

      comment' =
        result >>= \case
          Right resultValue ->
            case Aeson.fromJSON @Comment resultValue of
              Success c -> pure (Right c)
              Error e -> pure (Left $ ParseError e)
          Left e -> pure (Left $ UsageError e)
   in comment'

deleteComment :: Pool -> CurrentUserId -> CommentId -> IO (Either UsageError ())
deleteComment dbPool currentUserId commentId =
  let statement =
        [resultlessStatement|
          SELECT FROM api.delete_comment($1 :: UUID)
        |]

      commentId' = coerce @CommentId @UUID commentId

      currentUserId' = coerce @CurrentUserId @Int64 currentUserId
   in Pool.use dbPool $
        DB.authQuery currentUserId' commentId' statement
