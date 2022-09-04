{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Server (app) where

import Hasql.Pool (Pool)
import HoliplanWeb.Auth (authHandler)
import HoliplanWeb.Routes (holiplanAPI, sessionServer, planServer)
import Network.Wai (Request)
import Servant (type (:<|>) ((:<|>)))
import Servant.Server as Server
import Servant.Server.Experimental.Auth (AuthHandler)
import Holiplan.Session (UserSession)

app :: Pool -> Application
app dbPool =
  Server.serveWithContext
    holiplanAPI
    (genAuthServerContext dbPool)
    (planServer dbPool :<|> sessionServer dbPool)

genAuthServerContext :: Pool -> Context (AuthHandler Request UserSession ': '[])
genAuthServerContext dbPool = authHandler dbPool :. EmptyContext
