{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Repository.Tools.Statements (
    addTool,
    getTool,
    getTools,
    removeToolById,
    updateTool,
    removeTags,
    addTags
) where
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Typed (PGConnection, pgQuery, pgSQL)
import Database.PostgreSQL.Typed.Array
import Database.PostgreSQL.Typed.Query (PGSimpleQuery)
import Database.PostgreSQL.Typed.Types
import Lib.Database (settings, useTPGDatabase)
import Prelude hiding (words)

useTPGDatabase settings

getTools :: PGConnection -> UUID -> IO [(UUID, Text, Maybe Text,Maybe [Maybe Text])]
getTools conn user_id =
    pgQuery conn [pgSQL|
        SELECT      tool_id,
                    tl.name,
                    tl.description,
                    array_agg(tg.name)
        FROM        hello.tools tl
        LEFT JOIN   hello.tags  tg      USING (tool_id)
        WHERE       tl.user_id = ${user_id}
        GROUP BY    tool_id
    |]


getTool :: PGConnection -> UUID -> UUID -> IO [(UUID, Text, Maybe Text,Maybe [Maybe Text])]
getTool conn user_id tool_id =
    pgQuery conn [pgSQL|
        SELECT      tool_id,
                    tl.name,
                    tl.description,
                    array_agg(tg.name)
        FROM        hello.tools tl
        LEFT JOIN   hello.tags  tg      USING (tool_id)
        WHERE tool_id = ${tool_id} AND user_id = ${user_id}
        GROUP BY    tool_id
    |]



addTool :: PGConnection -> UUID -> Text -> Maybe Text -> IO [UUID]
addTool conn user_id name description =
    pgQuery conn [pgSQL|
        INSERT INTO hello.tools (name, description, user_id)
        VALUES (${name}, ${description}, ${user_id})
        RETURNING tool_id
    |]

updateTool :: PGConnection -> UUID -> UUID -> Text -> Maybe Text -> IO [()]
updateTool conn user_id id name description =
    pgQuery conn [pgSQL|
        UPDATE hello.tools
        SET name = ${name},
            description = ${description}
        WHERE tool_id = ${id} AND user_id = ${user_id}
    |]

removeToolById :: PGConnection -> UUID -> UUID -> IO [()]
removeToolById conn user_id tool_id =
    pgQuery conn [pgSQL|
        DELETE FROM hello.tools
        WHERE tool_id = ${tool_id} AND user_id = ${user_id}
    |]


addTags :: PGConnection -> [UUID] -> [Text] -> IO [UUID]
addTags conn tools_ids names =
    pgQuery conn [pgSQL|
        INSERT INTO hello.tags (tool_id,name)
        SELECT * FROM UNNEST(${tools_ids}::uuid[], ${names}::text[])
        returning tool_id
    |]

removeTags :: PGConnection -> UUID -> [Text] -> IO [()]
removeTags conn tool_id tag_names =
    pgQuery conn [pgSQL|
        DELETE FROM hello.tags
        WHERE tool_id = ${tool_id} AND name = ANY(${tag_names})
    |]
