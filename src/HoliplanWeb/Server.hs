module HoliplanWeb.Server (app) where

import Servant (Application)
import Servant.Server as Server (serve)
import HoliplanWeb.Routes (holiplanAPI, server)

app :: Application
app = Server.serve holiplanAPI server
