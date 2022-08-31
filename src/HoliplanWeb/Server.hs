{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Server (app) where

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM (newTVarIO)
import Data.Int (Int64)
import Hasql.Pool (Pool)
import HoliplanWeb.Auth (authHandler)
import HoliplanWeb.Routes (holiplanAPI, server)
import Network.Wai (Request)
import Servant (Application)
import Servant.Server (Context (EmptyContext, (:.)))
import Servant.Server as Server
import Servant.Server.Experimental.Auth (AuthHandler)

app planDetails dbPool =
  Server.serveWithContext
    holiplanAPI
    (genAuthServerContext dbPool)
    (server planDetails dbPool)

genAuthServerContext :: Pool -> Context (AuthHandler Request Int64 ': '[])
genAuthServerContext dbPool = authHandler dbPool :. EmptyContext
