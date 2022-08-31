{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Server (app) where

import Data.Int (Int64)
import Hasql.Pool (Pool)
import HoliplanWeb.Auth (authHandler)
import HoliplanWeb.Routes (holiplanAPI, server)
import Network.Wai (Request)
import Servant.Server as Server
import Servant.Server.Experimental.Auth (AuthHandler)

app :: Pool -> Application
app dbPool =
  Server.serveWithContext
    holiplanAPI
    (genAuthServerContext dbPool)
    (server dbPool)

genAuthServerContext :: Pool -> Context (AuthHandler Request Int64 ': '[])
genAuthServerContext dbPool = authHandler dbPool :. EmptyContext
