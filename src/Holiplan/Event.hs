{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Holiplan.Event (
  Event (..),
  ReqEvent (..),
  createEvent,
  editEvent,
  deleteEvent,
) where

import qualified DB
import Data.Aeson (FromJSON, Result (Error, Success), ToJSON)
import qualified Data.Aeson as Aeson
import Data.Time (UTCTime)
import Hasql.Pool (Pool, UsageError)
import qualified Hasql.Pool as Pool
import Hasql.TH (resultlessStatement, singletonStatement)
import Holiplan.Error (Error (ParseError, UsageError))
import Holiplan.Id (EventId (EventId), PlanId (PlanId), UserId (UserId))

data ReqEvent = ReqEvent
  { name :: Text,
    start_time :: UTCTime,
    end_time :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

data Event = Event
  { id :: EventId,
    plan_id :: PlanId,
    user_id :: UserId,
    name :: Text,
    start_time :: UTCTime,
    end_time :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ReqEvent
instance ToJSON ReqEvent
instance FromJSON Event
instance ToJSON Event

createEvent :: Pool -> UserId -> PlanId -> ReqEvent -> IO (Either Error Event)
createEvent dbPool (UserId currentUserId) (PlanId planId) (ReqEvent name startDate endDate) =
  let statement =
        [singletonStatement|
          SELECT create_event :: JSONB
            FROM api.create_event
              ( $1 :: UUID
              , $2 :: TEXT
              , $3 :: TIMESTAMPTZ
              , $4 :: TIMESTAMPTZ
              )
        |]

      result =
        Pool.use dbPool $
          DB.authQuery
            currentUserId
            (planId, name, startDate, endDate)
            statement

      event =
        result >>= \case
          Right resultValue ->
            case Aeson.fromJSON @Event resultValue of
              Success event' -> pure (Right event')
              Error e -> liftIO (print e) >> pure (Left $ ParseError e)
          Left e -> liftIO (print e) >> pure (Left $ UsageError e)
   in event

editEvent :: Pool -> UserId -> PlanId -> EventId -> ReqEvent -> IO (Either Error Event)
editEvent dbPool (UserId currentUserId) _planId (EventId eventId) (ReqEvent name startDate endDate) =
  let statement =
        [singletonStatement|
          SELECT edit_event :: JSONB
            FROM api.edit_event
              ( $1 :: UUID
              , $2 :: TEXT
              , $3 :: TIMESTAMPTZ
              , $4 :: TIMESTAMPTZ
              )
        |]

      result =
        Pool.use dbPool $
          DB.authQuery
            currentUserId
            (eventId, name, startDate, endDate)
            statement

      event =
        result >>= \case
          Right resultValue ->
            case Aeson.fromJSON @Event resultValue of
              Success event' -> pure (Right event')
              Error e -> liftIO (print e) >> pure (Left $ ParseError e)
          Left e -> liftIO (print e) >> pure (Left $ UsageError e)
   in event

deleteEvent :: Pool -> UserId -> PlanId -> EventId -> IO (Either UsageError ())
deleteEvent dbPool currentUserId _planId (EventId eventId) =
  let statement =
        [resultlessStatement|
          SELECT FROM api.delete_event($1 :: UUID)
        |]

      currentUserId' = coerce @UserId @Int64 currentUserId
   in Pool.use dbPool $
        DB.authQuery currentUserId' eventId statement
