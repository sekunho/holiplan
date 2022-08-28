{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
  type (:<|>) ((:<|>)), ServerError (errBody), throwError, err404
 )

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM (atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Foldable as Foldable (find)
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
    :<|> "plans" :> Get '[JSON] [PlanDetail]
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

$(deriveJSON defaultOptions{fieldLabelModifier = drop 13} ''PlanDetail)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''Plan)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 9} ''ReqPlan)

server :: TVar [PlanDetail] -> Server HoliplanAPI
server tvar = do
  getPlan tvar
    :<|> listPlans tvar
    :<|> createPlan tvar
 where
  getPlan :: TVar [PlanDetail] -> Int -> Handler PlanDetail
  getPlan tvar planId = do
    planDetails <- liftIO $ STM.readTVarIO tvar

    let planDetail = Foldable.find ((== planId) . plan_detail_id) planDetails

    case planDetail of
      Just p -> pure p
      Nothing -> throwError (err404 { errBody = "Plan doesn't exist" })

  listPlans :: TVar [PlanDetail] -> Handler [PlanDetail]
  listPlans tvar = do
    liftIO $ STM.readTVarIO tvar

  createPlan :: TVar [PlanDetail] -> ReqPlan -> Handler PlanDetail
  createPlan tvar reqPlan = do
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