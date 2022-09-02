{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Server (app) where

import Hasql.Pool (Pool)
import HoliplanWeb.Auth (authHandler)
import HoliplanWeb.Routes (holiplanAPI, server)
import Network.Wai (Request)
import Servant.Server as Server
import Servant.Server.Experimental.Auth (AuthHandler)
import Holiplan.Session (CurrentUserId)

app :: Pool -> Application
app dbPool =
  Server.serveWithContext
    holiplanAPI
    (genAuthServerContext dbPool)
    (server dbPool)

genAuthServerContext :: Pool -> Context (AuthHandler Request CurrentUserId ': '[])
genAuthServerContext dbPool = authHandler dbPool :. EmptyContext
