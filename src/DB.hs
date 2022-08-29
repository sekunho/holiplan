module DB (pool) where

import qualified Hasql.Connection as Connection (settings)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool (acquire)

pool :: Int -> IO Pool
pool poolCapacity =
  Pool.acquire poolCapacity "host=localhost port=5432 dbname=holiplan"

