{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Handler.Plan (type PlanAPI, listPlans, createPlan) where

import Control.Monad.IO.Class (liftIO)
import Hasql.Pool (Pool)
import Holiplan.Plan (PlanIndex, ReqPlan, Plan)
import qualified Holiplan.Plan as Plan
import Holiplan.Session (CurrentUserId)
import Servant (throwError)
import Servant.API (ReqBody, type (:<|>), type (:>))
import Servant.API.ContentTypes (JSON)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.Verbs (Get, PostCreated)
import Servant.Server (Handler, ServerError (errBody), err500)
import Servant.Server.Experimental.Auth (AuthServerData)

type PlanAPI =
  "plans"
    :> AuthProtect "cookie-auth"
    :> Get '[JSON] PlanIndex
    :<|> "plans"
      :> AuthProtect "cookie-auth"
      :> ReqBody '[JSON] ReqPlan
      :> PostCreated '[JSON] Plan

type instance AuthServerData (AuthProtect "cookie-auth") = CurrentUserId

listPlans :: Pool -> CurrentUserId -> Handler PlanIndex
listPlans dbPool currentUserId = do
  result <- liftIO $ Plan.listPlans dbPool currentUserId

  case result of
    Right planIndex -> pure planIndex
    Left e ->
      liftIO (print e)
        >> throwError (err500 {errBody = "Something went wrong while fetching the data"})

createPlan :: Pool -> CurrentUserId -> ReqPlan -> Handler Plan
createPlan dbPool currentUserId body = do
  result <- liftIO $ Plan.createPlan dbPool currentUserId body

  case result of
    Right plan -> pure plan
    Left e -> do
      liftIO $ print e
      throwError (err500 {errBody = "Something went wrong while creating a plan"})
