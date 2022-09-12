{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Routes (holiplanAPI, planServer, sessionServer) where

import Servant (
  AuthProtect,
  Server,
  type (:<|>) ((:<|>)),
  type (:>),
 )

import Hasql.Pool (Pool)
import Holiplan.Session (UserSession)
import HoliplanWeb.Handler.Plan (PlanAPI)
import qualified HoliplanWeb.Handler.Plan as PlanHandler
import HoliplanWeb.Handler.Session (SessionAPI)
import qualified HoliplanWeb.Handler.Session as SessionHandler
import Servant.Server.Experimental.Auth (AuthServerData)

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
    :<|> PlanHandler.addComment dbPool
    :<|> PlanHandler.editComment dbPool
    :<|> PlanHandler.deleteComment dbPool
    :<|> PlanHandler.createEvent dbPool
    :<|> PlanHandler.editEvent dbPool
    :<|> PlanHandler.deleteEvent dbPool

sessionServer :: Pool -> Server SessionAPI
sessionServer dbPool =
  SessionHandler.register dbPool
    :<|> SessionHandler.login dbPool
    :<|> SessionHandler.logout dbPool

holiplanAPI :: Proxy HoliplanAPI
holiplanAPI = Proxy
