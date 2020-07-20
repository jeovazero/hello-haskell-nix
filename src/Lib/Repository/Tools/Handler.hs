{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib.Repository.Tools.Handler (
    addTool,
    removeToolById,
    getTools,
    getTool
) where
import Data.Text (Text)
import qualified Lib.Repository.Tools.Statements as S
import qualified Control.Exception as C
import Data.UUID (UUID)
import Database.PostgreSQL.Typed (pgQuery, pgTransaction, PGConnection, PGError)
import Lib.Repository.Tools.Data (Tool(..), NewTool(..))

headOrThrow :: [a] -> IO a
headOrThrow [] = C.throwIO $ C.ErrorCall "empty list"
headOrThrow (x:_) = pure x

safeHead [] = Nothing
safeHead (x:_) = Just x

addTool :: PGConnection -> NewTool -> IO UUID
addTool conn NewTool{ name, description, tags } = pgTransaction conn $ do
    toolRow <- S.addTool conn name description
    tool_id <- headOrThrow toolRow
    let tools_ids = fmap (const tool_id) tags
    S.addTags conn tools_ids tags
    pure tool_id

unboxMaybeList :: [a] -> [Maybe a] -> [a]
unboxMaybeList acc [] = acc
unboxMaybeList acc (Nothing:xs) = unboxMaybeList acc []
unboxMaybeList acc (Just x:xs) = unboxMaybeList (x:acc) xs

tool (a,b,c,d) = Tool a b c (maybe [] (unboxMaybeList []) d)

getTools :: PGConnection -> IO [Tool]
getTools conn = do
    rows <- S.getTools conn
    pure $ fmap tool rows


getTool :: PGConnection -> UUID -> IO (Maybe Tool)
getTool conn tool_id = do
    rows <- S.getTool conn tool_id
    pure $ fmap tool $ safeHead rows

removeToolById :: PGConnection -> UUID -> IO [()]
removeToolById = S.removeToolById
