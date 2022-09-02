module Main (main) where

import qualified DB (pool)
import HoliplanWeb.Server (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = DB.pool 20 >>= \dbPool -> run 8080 (app dbPool)
