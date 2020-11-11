{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
 
module Lib.Repository.Tools.Statements (
    addTool,
    getTool,
    getTools,
    removeToolById,
    updateTool,
    removeTags,
    addTags
) where
import Prelude hiding (words)
import Database.PostgreSQL.Typed (pgQuery, pgSQL, PGConnection)
import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.Array
import Database.PostgreSQL.Typed.Query (PGSimpleQuery)
import Data.UUID (UUID)
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
import Lib.Database (useTPGDatabase, settings)

useTPGDatabase settings

getTools :: PGConnection -> IO [(UUID, Text, Maybe Text,Maybe [Maybe Text])]
getTools conn =
    pgQuery conn [pgSQL|
        SELECT      tool_id,
                    tl.name,
                    tl.description,
                    array_agg(tg.name)
        FROM        hello.tools tl
        LEFT JOIN   hello.tags  tg      USING (tool_id)
        GROUP BY    tool_id
    |]


getTool :: PGConnection -> UUID -> IO [(UUID, Text, Maybe Text,Maybe [Maybe Text])]
getTool conn tool_id =
    pgQuery conn [pgSQL|
        SELECT      tool_id,
                    tl.name,
                    tl.description,
                    array_agg(tg.name)
        FROM        hello.tools tl
        LEFT JOIN   hello.tags  tg      USING (tool_id)
        WHERE tool_id = ${tool_id}
        GROUP BY    tool_id
    |]



addTool :: PGConnection -> Text -> Maybe Text -> IO [UUID]
addTool conn name description =
    pgQuery conn [pgSQL|
        INSERT INTO hello.tools (name, description)
        VALUES (${name}, ${description})
        RETURNING tool_id
    |]

updateTool :: PGConnection -> UUID -> Text -> Maybe Text -> IO [()]
updateTool conn id name description =
    pgQuery conn [pgSQL|
        UPDATE hello.tools
        SET name = ${name},
            description = ${description}
        WHERE tool_id = ${id}
    |]

removeToolById :: PGConnection -> UUID -> IO [()]
removeToolById conn tool_id =
    pgQuery conn [pgSQL|
        DELETE FROM hello.tools
        WHERE tool_id = ${tool_id}
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
