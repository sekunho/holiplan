{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Routes (holiplanAPI, server) where

import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (
  Options (fieldLabelModifier),
  defaultOptions,
 )
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text (pack, toLower)
import Data.Time (parseTimeM)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Format (defaultTimeLocale)
import GHC.Generics (Generic)

import Servant (
  Application,
  FromHttpApiData (parseUrlPiece),
  Handler,
  Proxy (Proxy),
  Server,
  ServerError (errBody),
  err404,
  err500,
  throwError,
  type (:<|>) ((:<|>)),
 )

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM (atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import qualified DB
import Data.Aeson (Result (Error, Success))
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable (find)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool (use)
import qualified Hasql.Session as Session
import Hasql.TH (singletonStatement, vectorStatement)
import qualified Hasql.Transaction.Sessions as Transaction.Sessions
import Servant.API (
  Capture,
  DeleteNoContent,
  FormUrlEncoded,
  Get,
  Header,
  Headers,
  JSON,
  OctetStream,
  PlainText,
  PostCreated,
  Put,
  QueryParam,
  ReqBody,
  (:<|>),
  (:>),
 )
import Servant.Server (serve)

type HoliplanAPI =
  "plans" :> Capture "plan_id" Int :> Get '[JSON] PlanDetail
    :<|> "plans" :> Get '[JSON] PlanIndex
    :<|> "plans" :> ReqBody '[JSON] ReqPlan :> PostCreated '[JSON] PlanDetail

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

foo =
  [singletonStatement|
    select list_plans :: jsonb from api.list_plans() |]

server :: TVar [PlanDetail] -> Pool -> Server HoliplanAPI
server tvar dbPool = do
  getPlan tvar dbPool
    :<|> listPlans dbPool
    :<|> createPlan tvar dbPool
 where
  getPlan :: TVar [PlanDetail] -> Pool -> Int -> Handler PlanDetail
  getPlan tvar dbPool planId = do
    planDetails <- liftIO $ STM.readTVarIO tvar

    let planDetail = Foldable.find ((== planId) . plan_detail_id) planDetails

    case planDetail of
      Just p -> pure p
      Nothing -> throwError (err404 {errBody = "Plan doesn't exist"})

  listPlans :: Pool -> Handler PlanIndex
  listPlans dbPool = do
    -- let statement = Session.statement () foo
    res <- liftIO $ Pool.use dbPool (DB.authQuery Nothing () foo)

    case res of
      Left e -> do
        liftIO (print e)
        throwError (err500 {errBody = "shit's fucked"})
      Right t -> do
        let foo = Aeson.fromJSON @PlanIndex t

        case foo of
          Success pi -> pure pi
          Error t -> do
            throwError (err500 {errBody = "bruh"})

  createPlan :: TVar [PlanDetail] -> Pool -> ReqPlan -> Handler PlanDetail
  createPlan tvar dbPool reqPlan = do
    liftIO . STM.atomically $ do
      currentDetails <- STM.readTVar tvar

      let newId = case currentDetails of
            [] -> 1
            c : _ -> plan_detail_id c + 1

      let newPlanDetail =
            PlanDetail
              { plan_detail_id = newId
              , plan_detail_name = req_plan_name reqPlan
              , plan_detail_description = req_plan_description reqPlan
              , plan_detail_date = req_plan_date reqPlan
              , plan_detail_events = []
              , plan_detail_comments = []
              }

      STM.writeTVar tvar (currentDetails ++ [newPlanDetail])

      pure newPlanDetail

holiplanAPI :: Proxy HoliplanAPI
holiplanAPI = Proxy
