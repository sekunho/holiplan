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
  type (:<|>) ((:<|>)),
 )

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM (atomically, writeTVar, readTVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
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
  "plans" :> Capture "plan_id" Int :> Get '[JSON] PlanDetails
    :<|> "plans" :> Get '[JSON] [PlanDetails]
    :<|> "plans" :> ReqBody '[JSON] ReqPlan :> PostCreated '[JSON] Plan

data PlanDetails = PlanDetails
  { plan_details_id :: Int
  , plan_details_date :: Day
  , plan_details_name :: Text
  , plan_details_description :: Text
  , plan_details_events :: [Text]
  , plan_details_comments :: [Text]
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

$(deriveJSON defaultOptions{fieldLabelModifier = drop 13} ''PlanDetails)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''Plan)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 9} ''ReqPlan)

planDetails =
  PlanDetails
    { plan_details_id = 1
    , plan_details_date =
        fromJust $
          parseTimeM True defaultTimeLocale "%Y-%m-%d" "2022-08-27"
    , plan_details_name = "Bob's Birthday Bash"
    , plan_details_description = "It's Bob's birthday!"
    , plan_details_events = []
    , plan_details_comments = []
    }

plan =
  Plan
    { plan_id = 1
    , plan_date =
        fromJust $
          parseTimeM True defaultTimeLocale "%Y-%m-%d" "2022-08-27"
    , plan_name = "Bob's Birthday Bash"
    , plan_description = "It's Bob's birthday!"
    }

server :: TVar [PlanDetails] -> Server HoliplanAPI
server tvar = do
  getPlan
    :<|> listPlans tvar
    :<|> createPlan tvar
 where
  getPlan :: Int -> Handler PlanDetails
  getPlan _ = do
    liftIO $ putStrLn "hey"

    pure planDetails

  listPlans :: TVar [PlanDetails] -> Handler [PlanDetails]
  listPlans tvar = do
    liftIO $ STM.readTVarIO tvar

  createPlan :: TVar [PlanDetails] -> ReqPlan -> Handler Plan
  createPlan tvar reqPlan = do
    liftIO . STM.atomically $ do
      currentDetails <- STM.readTVar tvar
      STM.writeTVar tvar (planDetails:currentDetails)

    liftIO $ print reqPlan

    pure plan

holiplanAPI :: Proxy HoliplanAPI
holiplanAPI = Proxy
