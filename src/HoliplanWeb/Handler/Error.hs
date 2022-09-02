{-# LANGUAGE FlexibleContexts #-}
module HoliplanWeb.Handler.Error (throw500) where

import Servant (throwError)
import Servant.Server (ServerError (errBody), err500)
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Except (MonadError)

throw500 :: MonadError ServerError m => ByteString -> m a
throw500 msg = throwError (err500 {errBody = msg})
