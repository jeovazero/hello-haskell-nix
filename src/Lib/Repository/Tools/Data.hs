{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Lib.Repository.Tools.Data (
    Tool(..),
    NewTool(..),
    UpdateTool(..),
    UpdateInfo(..),
    getUpdateInfo,
    decodeNewTool,
    decodeUpdateTool,
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
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import GHC.Records (getField)


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

data UpdateTool = UpdateTool {
  name :: Maybe Text,
  description :: Maybe Text,
  tags :: Maybe [Text]
} deriving (Show, Generic)


instance FromJSON NewTool where
    parseJSON (Object obj) =
        NewTool <$> obj .: "name"
                <*> obj .:? "description"
                <*> obj .: "tags"

    parseJSON invalid = typeMismatch "Object" invalid


instance FromJSON UpdateTool where
    parseJSON (Object obj) =
        UpdateTool <$> obj .:? "name"
                   <*> obj .:? "description"
                   <*> obj .:? "tags"

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

decodeUpdateTool :: ByteString -> Maybe UpdateTool
decodeUpdateTool = decode

data UpdateInfo = UpdateInfo {
  tagsToAdd :: [Text],
  tagsToRemove :: [Text],
  payload :: Tool
} deriving (Show)

-- old = ['A', 'B', 'C']
-- current = ['B', 'D', 'E', 'E', 'D']
-- diff old current = (['A', 'C'], ['D', 'E'])
-- to remove: ['A', 'C']
-- to add: ['D', 'E']

diff [] [] = ([], [])
diff old current = diff' current (Set.fromList old) Set.empty []

diff' :: Ord a => [a] -> Set.Set a -> Set.Set a -> [a] -> ([a], [a])
diff' [] oldSet _ toAdd = (Set.elems oldSet, toAdd)
diff' (x:xs) oldSet nextSet toAdd =
    let
        (oldSet', toAdd')
          | Set.member x oldSet = (Set.delete x oldSet, toAdd)
          | Set.member x nextSet = (oldSet, toAdd)
          | otherwise = (oldSet, x:toAdd)

        nextSet' = Set.insert x nextSet
    in
        diff' xs oldSet' nextSet' toAdd'

merge :: Tool -> UpdateTool -> Tool
merge tool utool =
    Tool {
      id = id tool,
      name = fromMaybe (getField @"name" tool) (getField @"name" utool),
          description = maybe (getField @"description" tool) Just (getField @"description" utool),
      tags = []
    }


getUpdateInfo :: Tool -> UpdateTool -> ([Text], [Text], Tool)
getUpdateInfo tool updateTool =
    let
        tags1 = getField @"tags" tool
        tags2 = fromMaybe [] $ getField @"tags" updateTool
        (toRemove, toAdd) = diff tags1 tags2
        payload = merge tool updateTool
    in (toAdd, toRemove, payload)
