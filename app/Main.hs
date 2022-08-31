module Main (main) where

import qualified Control.Concurrent.STM as STM (newTVarIO)
import qualified DB (pool)
import HoliplanWeb.Server (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  planDetails <- STM.newTVarIO []
  dbPool <- DB.pool 20

  run 8080 (app dbPool)
