{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Repository.Tools.Handler (
    addTool,
    removeToolById,
    updateTool,
    getTools,
    getTool
) where
import qualified Control.Exception as C
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Typed (PGConnection, PGError, pgQuery, pgTransaction)
import Lib.Exception (AppException(..), AppExceptionType(..), headOrThrow)
import Lib.Repository.Tools.Data (NewTool(..), Tool(..))
import qualified Lib.Repository.Tools.Statements as S

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

addTool :: PGConnection -> UUID -> NewTool -> IO UUID
addTool conn user_id NewTool{ name, description, tags } = pgTransaction conn $ do
    toolRow <- S.addTool conn user_id name description
    tool_id <-
        headOrThrow
            toolRow
            (AppException $ Unexpected "the addTool didn't return an ID")
    let tools_ids = fmap (const tool_id) tags
    S.addTags conn tools_ids tags
    pure tool_id

tool :: (UUID, Text, Maybe Text, Maybe [Maybe Text]) -> Tool
tool (a,b,c,d) = Tool a b c (maybe [] catMaybes d)

getTools :: PGConnection -> UUID -> IO [Tool]
getTools conn user_id = do
    rows <- S.getTools conn user_id
    pure $ fmap tool rows

getTool :: PGConnection -> UUID -> UUID -> IO (Maybe Tool)
getTool conn user_id tool_id = do
    rows <- S.getTool conn user_id tool_id
    pure $ fmap tool $ safeHead rows

removeToolById :: PGConnection -> UUID -> UUID -> IO [()]
removeToolById = S.removeToolById

updateTool :: PGConnection -> UUID -> Tool -> [Text] -> [Text] -> IO [()]
updateTool conn user_id Tool{ id, name, description } tagsToAdd tagsToRemove =
    pgTransaction conn $ do
        let tool_ids = fmap (const id) tagsToAdd
        S.addTags conn tool_ids tagsToAdd
        S.removeTags conn id tagsToRemove
        S.updateTool conn user_id id name description
