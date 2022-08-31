{-# LANGUAGE FlexibleContexts #-}

module HoliplanWeb.Auth (authHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Hasql.Pool (Pool)
import qualified Holiplan.Session as Session
import Network.Wai (Request (requestHeaders))
import Servant (Handler, ServerError (errBody), throwError)
import Servant.Server (err401, err500)
import Servant.Server.Experimental.Auth (AuthHandler)
import qualified Servant.Server.Experimental.Auth as Auth (mkAuthHandler)
import qualified TextShow as Text.Show
import Web.Cookie (parseCookies)

authHandler :: Pool -> AuthHandler Request Int64
authHandler dbPool = Auth.mkAuthHandler (authenticate dbPool)

authenticate :: Pool -> Request -> Handler Int64
authenticate dbPool req = do
  let cookie =
        maybeToEither "Missing cookie header" $
          lookup "cookie" $
            requestHeaders req

      sessionToken =
        cookie >>= \c ->
          maybeToEither
            "Missing token in cookie"
            (Text.Show.showt <$> lookup "session_token" (parseCookies c))

  case sessionToken of
    Left e -> throw401 e
    Right sessionToken' -> do
      result <- liftIO $ Session.getUserId dbPool sessionToken'

      case result of
        -- TODO: Improve error handling
        -- Should be able to discern between actual internal server
        -- errors and when it returns 0 rows.
        Left e -> do
          liftIO $ print e
          throw500 "Something terrible just happened"
        Right userId -> pure userId
 where
  throw401 msg = throwError $ err401 {errBody = msg}
  throw500 msg = throwError $ err500 {errBody = msg}
  maybeToEither e = maybe (Left e) Right
