module Main where

import HoliplanWeb.Server (app)
import Network.Wai.Handler.Warp (run)
import qualified Control.Concurrent.STM as STM (newTVarIO)

main :: IO ()
main = do
  planDetails <- STM.newTVarIO []
  run 8080 (app planDetails)
