{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module HoliplanWeb.Handler.Plan (
  type PlanAPI,
  listPlans,
  createPlan,
  getPlanDetail,
  editPlan,
  deletePlan,
) where

import Hasql.Pool (Pool)
import Holiplan.Plan (
  Error (ParseError, UsageError),
  Plan,
  PlanDetail,
  PlanId,
  PlanIndex,
  ReqPlan,
 )
import qualified Holiplan.Plan as Plan
import Holiplan.Session (UserSession (UserSession))
import HoliplanWeb.Handler.Error (throw500)
import Servant.API (ReqBody, type (:<|>), type (:>))
import Servant.API.Capture (Capture)
import Servant.API.ContentTypes (JSON, NoContent (NoContent))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.Verbs (DeleteNoContent, Get, Patch, PostCreated)
import Servant.Server (Handler)

type PlanAPI =
  -- GET /plans (requires session)
  AuthProtect "cookie-auth" :> Get '[JSON] PlanIndex
    -- POST /plans
    :<|> AuthProtect "cookie-auth"
      :> ReqBody '[JSON] ReqPlan
      :> PostCreated '[JSON] Plan
    -- GET /plans/:plan_id
    :<|> AuthProtect "cookie-auth"
      :> Capture "plan_id" PlanId
      :> Get '[JSON] PlanDetail
    -- PATCH /plans/:plan_id
    :<|> AuthProtect "cookie-auth"
      :> Capture "plan_id" PlanId
      :> ReqBody '[JSON] ReqPlan
      :> Patch '[JSON] Plan
    -- DELETE /plans/:plan_id
    :<|> AuthProtect "cookie-auth"
      :> Capture "plan_id" PlanId
      :> DeleteNoContent

listPlans :: Pool -> UserSession -> Handler PlanIndex
listPlans dbPool (UserSession currentUserId _ _) = do
  result <- liftIO $ Plan.listPlans dbPool currentUserId

  case result of
    Right planIndex -> pure planIndex
    Left _e -> throw500 "Unable to fetch list of plans"

createPlan :: Pool -> UserSession -> ReqPlan -> Handler Plan
createPlan dbPool (UserSession currentUserId _ _) body = do
  result <- liftIO $ Plan.createPlan dbPool currentUserId body

  case result of
    Right plan -> pure plan
    Left e ->
      case e of
        UsageError _ ->
          throw500 "Something terribly wrong has happened"
        ParseError _ ->
          throw500 "Something terribly wrong has happened: Failed to parse"

getPlanDetail :: Pool -> UserSession -> PlanId -> Handler PlanDetail
getPlanDetail dbPool (UserSession currentUserId _ _) planId = do
  -- I could probably just manually write an `FromHttpApiData` instance of `PlanId`
  result <- liftIO $ Plan.getPlanDetail dbPool currentUserId planId

  case result of
    Right planDetail -> pure planDetail
    Left _e -> throw500 "Unable to fetch plan detail"

editPlan :: Pool -> UserSession -> PlanId -> ReqPlan -> Handler Plan
editPlan dbPool (UserSession currentUserId _ _) planId body = do
  result <- liftIO $ Plan.editPlan dbPool currentUserId planId body

  case result of
    Right plan -> pure plan
    Left e ->
      case e of
        UsageError _ -> throw500 "Failed to update plan"
        ParseError _ -> throw500 "Failed to parse plan"

deletePlan :: Pool -> UserSession -> PlanId -> Handler NoContent
deletePlan dbPool (UserSession currentUserId _ _) planId = do
  result <- liftIO $ Plan.deletePlan dbPool currentUserId planId

  case result of
    Right _ -> pure NoContent
    Left _ -> throw500 "Failed to update plan"
