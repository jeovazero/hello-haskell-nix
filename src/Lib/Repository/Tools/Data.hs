{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib.Repository.Tools.Data (
    Tool(..),
    NewTool(..),
    decodeNewTool,
    encodeTool,
    encodeTools
) where

import Prelude hiding (id)
import Data.UUID (UUID)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)


data Tool = Tool {
  id :: UUID,
  name :: Text,
  description :: Maybe Text,
  tags :: [Text]
} deriving (Show, Generic)

data NewTool = NewTool {
  name :: Text,
  description :: Maybe Text,
  tags :: [Text]
} deriving (Show, Generic)


instance FromJSON NewTool where
    parseJSON (Object obj) =
        NewTool <$> obj .: "name"
                <*> obj .:? "description"
                <*> obj .: "tags"

    parseJSON invalid = typeMismatch "Object" invalid

instance ToJSON Tool where
    toEncoding Tool{..} = pairs (
           "id"          .= id
        <> "name"        .= name
        <> "description" .= description
        <> "tags"        .= tags
        )


encodeTool :: Tool -> ByteString
encodeTool = encode

encodeTools :: [Tool] -> ByteString
encodeTools = encode

decodeNewTool :: ByteString -> Maybe NewTool
decodeNewTool = decode
