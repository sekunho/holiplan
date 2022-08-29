module Main where

import HoliplanWeb.Server (app)
import Network.Wai.Handler.Warp (run)
import qualified Control.Concurrent.STM as STM (newTVarIO)
import qualified DB (pool)

main :: IO ()
main = do
  planDetails <- STM.newTVarIO []
  dbPool <- DB.pool 20

  run 8080 (app planDetails dbPool)
