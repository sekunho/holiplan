{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Handler.Plan (
  type PlanAPI,
  listPlans,
  createPlan,
  getPlanDetail,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.UUID (UUID)
import Hasql.Pool (Pool)
import Holiplan.Plan (Error (ParseError, UsageError), Plan, PlanDetail, PlanId, PlanIndex, ReqPlan)
import qualified Holiplan.Plan as Plan
import Holiplan.Session (CurrentUserId)
import HoliplanWeb.Handler.Error (throw500)
import Servant.API (ReqBody, type (:<|>), type (:>))
import Servant.API.Capture (Capture)
import Servant.API.ContentTypes (JSON)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.Verbs (Get, PostCreated)
import Servant.Server (Handler)
import Servant.Server.Experimental.Auth (AuthServerData)

type PlanAPI =
  "plans"
    :> AuthProtect "cookie-auth"
    :> Get '[JSON] PlanIndex
    :<|> "plans"
      :> AuthProtect "cookie-auth"
      :> ReqBody '[JSON] ReqPlan
      :> PostCreated '[JSON] Plan
    :<|> "plans"
      :> AuthProtect "cookie-auth"
      :> Capture "plan_id" UUID
      :> Get '[JSON] PlanDetail

type instance AuthServerData (AuthProtect "cookie-auth") = CurrentUserId

listPlans :: Pool -> CurrentUserId -> Handler PlanIndex
listPlans dbPool currentUserId = do
  result <- liftIO $ Plan.listPlans dbPool currentUserId

  case result of
    Right planIndex -> pure planIndex
    Left _e -> throw500 "Unable to fetch list of plans"

createPlan :: Pool -> CurrentUserId -> ReqPlan -> Handler Plan
createPlan dbPool currentUserId body = do
  result <- liftIO $ Plan.createPlan dbPool currentUserId body

  case result of
    Right plan -> pure plan
    Left e ->
      case e of
        UsageError _ ->
          throw500 "Something terribly wrong has happened"
        ParseError _ ->
          throw500 "Something terribly wrong has happened: Failed to parse"

getPlanDetail :: Pool -> CurrentUserId -> UUID -> Handler PlanDetail
getPlanDetail dbPool currentUserId planId = do
  -- I could probably just manually write an `FromHttpApiData` instance of `PlanId`
  let planId' = coerce @UUID @PlanId planId
  result <- liftIO $ Plan.getPlanDetail dbPool currentUserId planId'

  case result of
    Right planDetail -> pure planDetail
    Left _e -> throw500 "Unable to fetch plan detail"
