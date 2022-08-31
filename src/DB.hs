{-# LANGUAGE QuasiQuotes #-}

module DB (transaction, pool, query, authQuery) where

import Data.Int (Int64)
import Data.Text (Text)
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

pool :: Int -> IO Pool
pool poolCapacity =
  Pool.acquire poolCapacity "host=localhost port=5432 dbname=holiplan"

query :: forall a b. a -> Statement a b -> Session b
query param statement = transaction $ Transaction.statement param statement

-- | Runs a query in an authenticated setting.
authQuery :: Int64 -> a -> Statement a b -> Session b
authQuery userId param statement = do
  let setAnonRole = Transaction.sql "SET LOCAL ROLE hp_anon"

      setUserId =
        Transaction.statement
          (Text.Show.showt userId)
          [resultlessStatement|
              SELECT set_config('request.session_user_id', $1 :: TEXT, true) :: TEXT
              |]

      authenticate =
        Transaction.sql "SELECT auth.authenticate()"

  transaction $
    setAnonRole
      >> setUserId
      >> authenticate
      >> Transaction.statement param statement

transaction :: Transaction a -> Session a
transaction = Transaction.Sessions.transaction ReadCommitted Write
