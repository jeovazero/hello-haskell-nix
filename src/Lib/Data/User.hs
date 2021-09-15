{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Data.User (
    User(..),
    NewUser(..),
    UserCredentials(..),
    decodeNewUser,
    decodeUserCredentials
) where

import Data.Aeson
    ( decode, (.:), FromJSON(parseJSON), Value(Object), withText, FromJSONKey )
import Data.Aeson.Types (typeMismatch)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data User
  = User
      { userId   :: UUID
      , name     :: Text
      , email    :: Text
      , password :: Text
      }
  deriving (Show, Generic)

data NewUser
  = NewUser
      { nName     :: Text
      , nEmail    :: Text
      , nPassword :: Text
      }
  deriving (Show)

instance FromJSON NewUser where
    parseJSON (Object obj) =
        NewUser <$> obj .: "name"
                <*> obj .: "email"
                <*> obj .: "password"

    parseJSON invalid = typeMismatch "Object" invalid

data UserCredentials
  = UserCredentials
      { cEmail    :: Text
      , cPassword :: Text
      }
  deriving (Show)

instance FromJSON UserCredentials where
    parseJSON (Object obj) =
        UserCredentials <$> obj .: "email"
                        <*> obj .: "password"

    parseJSON invalid = typeMismatch "Object" invalid

decodeNewUser :: ByteString -> Maybe NewUser
decodeNewUser = decode

decodeUserCredentials :: ByteString -> Maybe UserCredentials
decodeUserCredentials = decode
