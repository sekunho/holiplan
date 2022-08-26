module Main where

import Routes (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  run 8080 app
