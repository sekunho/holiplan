{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Routes (app) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Data.Text as Text (pack, toLower)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant (Application, FromHttpApiData (parseUrlPiece), Handler, Proxy (Proxy), Server)
import Servant.API (Capture, DeleteNoContent, FormUrlEncoded, Get, Header, Headers, JSON, OctetStream, PlainText, Put, QueryParam, ReqBody, (:<|>), (:>))
import Servant.Server (serve)

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name
  deriving stock (Eq, Show, Generic)

data User = User
  { name :: String
  , age :: Int
  , email :: String
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON User

instance FromHttpApiData SortBy where
  parseUrlPiece txt =
    case Text.toLower txt of
      "age" -> pure Age
      "name" -> pure Name
      _ -> Left $ Text.pack "oh no"

users1 :: [User]
users1 =
  [ User "Isaac Newton" 372 "isaac@newton.co.uk"
  , User "Albert Einstein" 136 "ae@mc2.org"
  ]

server1 :: Server UserAPI
server1 = users
 where
  users :: Maybe SortBy -> Handler [User]
  users _ = return users1

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server1
