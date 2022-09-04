{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module HoliplanWeb.Routes (holiplanAPI, planServer, sessionServer) where

import Servant (
  Server,
  type (:<|>) ((:<|>)),
  type (:>), AuthProtect
 )

import Hasql.Pool (Pool)
import HoliplanWeb.Handler.Plan (PlanAPI)
import qualified HoliplanWeb.Handler.Plan as PlanHandler
import HoliplanWeb.Handler.Session (SessionAPI)
import qualified HoliplanWeb.Handler.Session as SessionHandler
import Servant.Server.Experimental.Auth (AuthServerData)
import Holiplan.Session (UserSession)

type HoliplanAPI =
  "plans" :> PlanAPI
    :<|> "session" :> SessionAPI

type instance AuthServerData (AuthProtect "cookie-auth") = UserSession

planServer :: Pool -> Server PlanAPI
planServer dbPool =
  PlanHandler.listPlans dbPool
    :<|> PlanHandler.createPlan dbPool
    :<|> PlanHandler.getPlanDetail dbPool
    :<|> PlanHandler.editPlan dbPool
    :<|> PlanHandler.deletePlan dbPool

sessionServer :: Pool -> Server SessionAPI
sessionServer dbPool =
  SessionHandler.register dbPool
    :<|> SessionHandler.login dbPool
    :<|> SessionHandler.logout dbPool

holiplanAPI :: Proxy HoliplanAPI
holiplanAPI = Proxy
