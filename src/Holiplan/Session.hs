{-# LANGUAGE QuasiQuotes #-}

module Holiplan.Session (
  CurrentUserId (CurrentUserId),
  getUserId,
) where

import qualified DB
import Hasql.Pool (Pool, UsageError)
import qualified Hasql.Pool as Pool
import Hasql.TH (singletonStatement)
import qualified Hasql.Transaction as Transaction

newtype CurrentUserId = CurrentUserId Int64

getUserId :: Pool -> Text -> IO (Either UsageError Int64)
getUserId dbPool sessionToken = do
  let setUserRole = Transaction.sql "SET LOCAL ROLE hp_anon"
      fetchUserId =
        Transaction.statement
          sessionToken
          [singletonStatement|
            SELECT session_user_id :: BIGINT
              FROM auth.session_user_id($1 :: TEXT)
            |]

  liftIO $
    Pool.use
      dbPool
      (DB.transaction $ setUserRole >> fetchUserId)
