{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HoliplanWeb.Handler.Session (
  SessionAPI,
  register,
  login,
  logout,
) where

import qualified Data.Text.Encoding as Text
import qualified Data.Time.Clock.POSIX as Time
import Hasql.Pool (Pool)
import Holiplan.Session (Creds, UserSession (UserSession))
import qualified Holiplan.Session as Session
import HoliplanWeb.Handler.Error (throw500)
import Servant.API (Header, Headers, ReqBody, type (:<|>), type (:>))
import qualified Servant.API as API
import Servant.API.ContentTypes (JSON, NoContent (NoContent))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.Verbs (Delete, Post, PostNoContent)
import Servant.Server (Handler)
import Web.Cookie (
  SetCookie (
    setCookieExpires,
    setCookieHttpOnly,
    setCookieName,
    setCookiePath,
    setCookieSameSite,
    setCookieValue
  ),
  defaultSetCookie,
  sameSiteStrict,
 )

type SessionAPI =
  -- POST /session/register
  "register" :> ReqBody '[JSON] Creds :> PostNoContent
    :<|> "login"
      :> ReqBody '[JSON] Creds
      :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] NoContent)
    :<|> "logout"
      :> AuthProtect "cookie-auth"
      :> Delete '[JSON] (Headers '[Header "Set-Cookie" SetCookie] NoContent)

register :: Pool -> Creds -> Handler NoContent
register dbPool creds = do
  result <- liftIO $ Session.register dbPool creds

  case result of
    Right _ -> pure NoContent
    Left _ -> throw500 "Unable to register"

login :: Pool -> Creds -> Handler (Headers '[Header "Set-Cookie" SetCookie] NoContent)
login dbPool creds = do
  result <- liftIO $ Session.login dbPool creds

  case result of
    Right (UserSession _ expiresOn token) ->
      let sessionCookie :: SetCookie
          sessionCookie =
            defaultSetCookie
              { setCookieName = "session_token",
                setCookieValue = Text.encodeUtf8 token,
                setCookiePath = Just "/",
                setCookieExpires = Just expiresOn,
                setCookieHttpOnly = True,
                setCookieSameSite = Just sameSiteStrict
              }
       in pure (API.addHeader sessionCookie NoContent)
    Left e ->
      liftIO (print e)
        >> throw500 "Unable to login"

logout :: Pool -> UserSession -> Handler (Headers '[Header "Set-Cookie" SetCookie] NoContent)
logout dbPool userSession = do
  result <- liftIO $ Session.logout dbPool userSession

  let sessionCookie :: SetCookie
      sessionCookie =
        defaultSetCookie
          { setCookieName = "session_token",
            setCookieValue = "",
            setCookiePath = Just "/",
            setCookieHttpOnly = True,
            setCookieExpires = Just (Time.posixSecondsToUTCTime 0),
            setCookieSameSite = Just sameSiteStrict
          }

  case result of
    Right _ -> pure (API.addHeader sessionCookie NoContent)
    Left e ->
      liftIO (print e) >>
      throw500 "Unable to logout"
