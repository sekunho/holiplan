module Holiplan.Id (
  PlanId (PlanId),
  CommentId (CommentId),
  UserId (UserId),
  EventId (EventId),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.UUID (UUID)
import Servant.API (FromHttpApiData)

newtype CommentId = CommentId UUID
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, FromHttpApiData) via UUID

newtype PlanId = PlanId UUID
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, FromHttpApiData) via UUID

newtype EventId = EventId UUID
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, FromHttpApiData) via UUID

newtype UserId = UserId Int64
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, FromHttpApiData) via Int64
