{-# LANGUAGE FlexibleContexts #-}

module HoliplanWeb.Auth (authHandler) where

import qualified Data.List as List
import qualified Data.Text.Encoding as Text
import Hasql.Pool (Pool)
import Holiplan.Session (CurrentUserId)
import qualified Holiplan.Session as Session
import Network.Wai (Request (requestHeaders))
import Servant (Handler, ServerError (errBody), throwError)
import Servant.Server (err401, err500)
import Servant.Server.Experimental.Auth (AuthHandler)
import qualified Servant.Server.Experimental.Auth as Auth (mkAuthHandler)
import Web.Cookie (parseCookies)

authHandler :: Pool -> AuthHandler Request CurrentUserId
authHandler dbPool = Auth.mkAuthHandler (authenticate dbPool)

authenticate :: Pool -> Request -> Handler CurrentUserId
authenticate dbPool req = do
  let cookie =
        maybeToEither "Missing cookie header" $
          List.lookup "cookie" $
            requestHeaders req

      sessionToken =
        cookie >>= \c ->
          maybeToEither
            "Missing token in cookie"
            (Text.decodeUtf8 <$> List.lookup "session_token" (parseCookies c))

  case sessionToken of
    Left e -> throw401 e
    Right sessionToken' -> do
      result <- liftIO $ do
        print sessionToken'
        Session.getUserId dbPool sessionToken'

      case result of
        -- TODO: Improve error handling
        -- Should be able to discern between actual internal server
        -- errors and when it returns 0 rows.
        Left e -> do
          liftIO $ print e
          throw500 "Something terrible just happened"
        Right userId -> pure (coerce @Int64 @CurrentUserId userId)
 where
  throw401 msg = throwError $ err401 {errBody = msg}
  throw500 msg = throwError $ err500 {errBody = msg}
  maybeToEither e = maybe (Left e) Right
