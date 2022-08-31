{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Handler.Plan (type PlanAPI, listPlans) where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Hasql.Pool (Pool)
import Holiplan.Plan (PlanIndex)
import qualified Holiplan.Plan as Plan
import Servant (throwError)
import Servant.API (type (:>))
import Servant.API.ContentTypes (JSON)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.Verbs (Get)
import Servant.Server (Handler, ServerError (errBody), err500)
import Servant.Server.Experimental.Auth (AuthServerData)

type PlanAPI = "plans" :> AuthProtect "cookie-auth" :> Get '[JSON] PlanIndex

type instance AuthServerData (AuthProtect "cookie-auth") = Int64

listPlans :: Pool -> Int64 -> Handler PlanIndex
listPlans dbPool userId = do
  result <- liftIO $ Plan.listPlans dbPool userId

  case result of
    Right planIndex -> pure planIndex
    Left _ ->
      throwError (err500 {errBody = "Something went wrong while fetching the data"})
