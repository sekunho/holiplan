{-# LANGUAGE QuasiQuotes #-}

module DB (pool, query, authQuery) where

import qualified Hasql.Connection as Connection (settings)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import Hasql.TH (resultlessStatement)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (Write))
import qualified Hasql.Transaction.Sessions as Transaction.Sessions
import qualified TextShow as Text.Show
import Data.Text (Text)

pool :: Int -> IO Pool
pool poolCapacity =
  Pool.acquire poolCapacity "host=localhost port=5432 dbname=holiplan"

query :: forall a b. a -> Statement a b -> Session b
query param statement = transaction $ Transaction.statement param statement

-- | Runs a query in an authenticated setting.
authQuery :: Maybe Text -> a -> Statement a b -> Session b
authQuery sessionToken param statement = do
  let setAnonRole = Transaction.sql "SET LOCAL ROLE hp_anon"

      setUserId =
        case sessionToken of
          Just sessionToken' ->
            Transaction.statement
              sessionToken'
              [resultlessStatement|
              SELECT set_config('request.session_token', $1 :: TEXT, true) :: TEXT
              |]

          Nothing -> pure ()

      authenticate =
        Transaction.sql "SELECT auth.authenticate()"

  transaction $
    setAnonRole
      >> setUserId
      >> authenticate
      >> Transaction.statement param statement

transaction :: Transaction a -> Session a
transaction = Transaction.Sessions.transaction ReadCommitted Write
