{-# LANGUAGE QuasiQuotes #-}

module Holiplan.Session (getUserId) where

import Control.Monad.IO.Class (liftIO)
import qualified DB
import Data.Int (Int64)
import Data.Text (Text)
import Hasql.Pool (Pool, UsageError)
import qualified Hasql.Pool as Pool
import Hasql.TH (singletonStatement)
import qualified Hasql.Transaction as Transaction

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
