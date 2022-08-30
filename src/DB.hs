{-# LANGUAGE QuasiQuotes #-}

module DB (pool, authQuery) where

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

authQuery :: Maybe Int -> a -> Statement a b -> Session b
authQuery userId param statement = do
  let setAnonRole = Transaction.sql "SET LOCAL ROLE hp_anon"

      setUserId =
        case userId of
          Just userId' ->
            Transaction.statement
              (Text.Show.showt <$> userId)
              [resultlessStatement|
              SELECT set_config('claims.user_id', $1 :: TEXT?, true) :: TEXT
              |]

          Nothing -> pure ()

      authenticate =
        Transaction.sql "SELECT auth.authenticate()"

  Transaction.Sessions.transaction ReadCommitted Write $
    setAnonRole
      >> setUserId
      >> authenticate
      >> Transaction.statement param statement
