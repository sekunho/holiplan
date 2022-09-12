{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Handler.Plan (
  type PlanAPI,
  listPlans,
  createPlan,
  getPlanDetail,
  editPlan,
  deletePlan,
  addComment,
  editComment,
  deleteComment,
  createEvent,
  editEvent,
  deleteEvent,
) where

import Hasql.Pool (Pool)
import Holiplan.Error (Error (ParseError, UsageError))
import Holiplan.Event (Event, ReqEvent)
import qualified Holiplan.Event as Event
import Holiplan.Id (CommentId, EventId, PlanId)
import Holiplan.Plan (
  Comment,
  Plan,
  PlanDetail,
  PlanIndex,
  ReqComment,
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
    :<|> AuthProtect "cookie-auth"
      :> Capture "plan_id" PlanId
      :> "comments"
      :> ReqBody '[JSON] ReqComment
      :> PostCreated '[JSON] Comment
    :<|> AuthProtect "cookie-auth"
      :> Capture "plan_id" PlanId
      :> "comments"
      :> Capture "comment_id" CommentId
      :> ReqBody '[JSON] ReqComment
      :> Patch '[JSON] Comment
    :<|> AuthProtect "cookie-auth"
      :> Capture "plan_id" PlanId
      :> "comments"
      :> Capture "comment_id" CommentId
      :> DeleteNoContent
    :<|> AuthProtect "cookie-auth"
      :> Capture "plan_id" PlanId
      :> "events"
      :> ReqBody '[JSON] ReqEvent
      :> PostCreated '[JSON] Event
    :<|> AuthProtect "cookie-auth"
      :> Capture "plan_id" PlanId
      :> "events"
      :> Capture "event_id" EventId
      :> ReqBody '[JSON] ReqEvent
      :> PostCreated '[JSON] Event
    :<|> AuthProtect "cookie-auth"
      :> Capture "plan_id" PlanId
      :> "events"
      :> Capture "event_id" EventId
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

addComment :: Pool -> UserSession -> PlanId -> ReqComment -> Handler Comment
addComment dbPool (UserSession currentUserId _ _) planId body = do
  result <- liftIO $ Plan.addComment dbPool currentUserId body planId

  case result of
    Right comment -> pure comment
    Left e ->
      case e of
        UsageError _ -> throw500 "Failed to comment"
        ParseError _ -> throw500 "Failed to parse comment"

editComment ::
  Pool ->
  UserSession ->
  PlanId ->
  CommentId ->
  ReqComment ->
  Handler Comment
editComment dbPool (UserSession currentUserId _ _) _planId commentId body = do
  result <- liftIO $ Plan.editComment dbPool currentUserId commentId body

  case result of
    Right comment -> pure comment
    Left e ->
      case e of
        UsageError _ -> throw500 "Failed to edit comment"
        ParseError _ -> throw500 "Failed to parse comment"

deleteComment :: Pool -> UserSession -> PlanId -> CommentId -> Handler NoContent
deleteComment dbPool (UserSession currentUserId _ _) _planId commentId = do
  result <- liftIO $ Plan.deleteComment dbPool currentUserId commentId

  case result of
    Right _ -> pure NoContent
    Left _ -> throw500 "Failed to delete comment"

createEvent :: Pool -> UserSession -> PlanId -> ReqEvent -> Handler Event
createEvent dbPool (UserSession currentUserId _ _) planId body = do
  result <- liftIO $ Event.createEvent dbPool currentUserId planId body

  case result of
    Right event -> pure event
    Left e ->
      case e of
        UsageError _ -> throw500 "Failed to create event"
        ParseError _ -> throw500 "Failed to parse event"

editEvent :: Pool -> UserSession -> PlanId -> EventId -> ReqEvent -> Handler Event
editEvent dbPool (UserSession currentUserId _ _) planId eventId body = do
  result <- liftIO $ Event.editEvent dbPool currentUserId planId eventId body

  case result of
    Right event -> pure event
    Left e ->
      case e of
        UsageError _ -> throw500 "Failed to edit event"
        ParseError _ -> throw500 "Failed to parse event"

deleteEvent :: Pool -> UserSession -> PlanId -> EventId -> Handler NoContent
deleteEvent dbPool (UserSession currentUserId _ _) planId eventId = do
  result <- liftIO $ Event.deleteEvent dbPool currentUserId planId eventId

  case result of
    Right _ -> pure NoContent
    Left _ -> throw500 "Failed to delete event"
