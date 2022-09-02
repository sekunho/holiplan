{-# LANGUAGE ExplicitNamespaces #-}

module HoliplanWeb.Routes (holiplanAPI, server) where

import Servant (
  Server,
  type (:<|>) ((:<|>)),
 )

import Hasql.Pool (Pool)
import HoliplanWeb.Handler.Plan (PlanAPI)
import qualified HoliplanWeb.Handler.Plan as PlanHandler

type HoliplanAPI = PlanAPI

server :: Pool -> Server HoliplanAPI
server dbPool =
  PlanHandler.listPlans dbPool
    :<|> PlanHandler.createPlan dbPool
    :<|> PlanHandler.getPlanDetail dbPool
    :<|> PlanHandler.editPlan dbPool
    :<|> PlanHandler.deletePlan dbPool

holiplanAPI :: Proxy HoliplanAPI
holiplanAPI = Proxy
