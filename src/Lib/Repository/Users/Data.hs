{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib.Repository.Users.Data (
    User(..),
    NewUser(..),
    UserCredentials(..),
    decodeNewUser,
    decodeUserCredentials
) where

import Data.UUID (UUID)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)


data User = User {
  userId :: UUID,
  name :: Text,
  email :: Text,
  password :: Text
} deriving (Show, Generic)

data NewUser = NewUser {
  name :: Text,
  email :: Text,
  password :: Text
} deriving (Show, Generic)

instance FromJSON NewUser where
    parseJSON (Object obj) =
        NewUser <$> obj .: "name"
                <*> obj .: "email"
                <*> obj .: "password"

    parseJSON invalid = typeMismatch "Object" invalid


data UserCredentials = UserCredentials {
  email :: Text,
  password :: Text
} deriving (Show, Generic)

instance FromJSON UserCredentials where
    parseJSON (Object obj) =
        UserCredentials <$> obj .: "email"
                        <*> obj .: "password"

    parseJSON invalid = typeMismatch "Object" invalid

decodeNewUser :: ByteString -> Maybe NewUser
decodeNewUser = decode

decodeUserCredentials :: ByteString -> Maybe UserCredentials
decodeUserCredentials = decode
