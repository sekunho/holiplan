module HoliplanWeb.Routes (holiplanAPI, server) where

import Servant (
  Proxy (Proxy),
  Server,
 )

import Hasql.Pool (Pool)
import HoliplanWeb.Handler.Plan (PlanAPI)
import qualified HoliplanWeb.Handler.Plan as Handler.Plan

type HoliplanAPI = PlanAPI

server :: Pool -> Server HoliplanAPI
server = Handler.Plan.listPlans

holiplanAPI :: Proxy HoliplanAPI
holiplanAPI = Proxy
