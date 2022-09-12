{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Holiplan.Session (
  Creds (..),
  Username (..),
  Password (..),
  UserSession (..),
  getSessionByToken,
  register,
  login,
  logout,
) where

import qualified DB
import Data.Aeson (FromJSON, Result (Error, Success), ToJSON)
import qualified Data.Aeson as Aeson
import Data.Time (UTCTime)
import Hasql.Pool (Pool, UsageError)
import qualified Hasql.Pool as Pool
import Hasql.Statement (Statement)
import Hasql.TH (resultlessStatement, singletonStatement)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as Transaction
import Holiplan.Id (UserId (UserId))

newtype Username = Username Text
  deriving (FromJSON, ToJSON) via Text

newtype Password = Password Text
  deriving (FromJSON, ToJSON) via Text

data Creds = Creds
  { username :: Username,
    password :: Password
  }
  deriving stock (Generic)

instance ToJSON Creds
instance FromJSON Creds

data UserSession = UserSession
  { user_id :: UserId,
    expires_on :: UTCTime,
    token :: Text
  }
  deriving stock (Generic)

instance ToJSON UserSession
instance FromJSON UserSession

data Error
  = UsageError UsageError
  | ParseError String
  deriving stock (Show)

getSessionByToken :: Pool -> Text -> IO (Either Error UserSession)
getSessionByToken dbPool sessionToken = do
  let setUserRole = Transaction.sql "SET LOCAL ROLE hp_anon"
      fetchSession =
        Transaction.statement
          sessionToken
          [singletonStatement|
            SELECT get_user_session :: JSONB
              FROM auth.get_user_session($1 :: TEXT)
            |]

  result <-
    liftIO $
      Pool.use
        dbPool
        (DB.transaction $ setUserRole >> fetchSession)

  case result of
    Right resultValue ->
      case Aeson.fromJSON @UserSession resultValue of
        Success session -> pure . pure $ session
        Error e -> pure (Left $ ParseError e)
    Left e -> pure (Left $ UsageError e)

register :: Pool -> Creds -> IO (Either UsageError ())
register dbPool (Creds uname pw) = do
  let uname' :: Text
      uname' = coerce @Username @Text uname

      pw' :: Text
      pw' = coerce @Password @Text pw

      transaction :: Transaction ()
      transaction =
        Transaction.statement
          (uname', pw')
          [singletonStatement|
            SELECT FROM api.register($1 :: TEXT, $2 :: TEXT)
          |]

  liftIO $
    Pool.use dbPool $
      DB.transaction transaction

login :: Pool -> Creds -> IO (Either Error UserSession)
login dbPool (Creds uname pw) =
  let uname' :: Text
      uname' = coerce @Username @Text uname

      pw' :: Text
      pw' = coerce @Password @Text pw

      statement =
        [singletonStatement|
              SELECT login :: JSONB
                FROM api.login($1 :: TEXT, $2 :: TEXT)
            |]

      result =
        Pool.use dbPool (DB.query (uname', pw') statement)

      session =
        result >>= \case
          Right resultValue ->
            case Aeson.fromJSON @UserSession resultValue of
              Success session' -> pure (Right session')
              Error e -> pure (Left $ ParseError e)
          Left e -> pure (Left $ UsageError e)
   in session

logout :: Pool -> UserSession -> IO (Either UsageError ())
logout dbPool (UserSession currentUserId _ sessionToken) =
  let statement :: Statement Text ()
      statement =
        [resultlessStatement|
            SELECT FROM auth.logout($1 :: TEXT)
          |]

      currentUserId' = coerce @UserId @Int64 currentUserId

      result =
        Pool.use dbPool $
          DB.authQuery currentUserId' sessionToken statement
   in result
