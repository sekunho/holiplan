module HoliplanWeb.Server (app) where

import Servant (Application)
import Servant.Server as Server (serve)
import HoliplanWeb.Routes (holiplanAPI, server)
import qualified Control.Concurrent.STM as STM (newTVarIO)
import Control.Concurrent.STM (TVar)

app planDetails =
  Server.serve holiplanAPI (server planDetails)
