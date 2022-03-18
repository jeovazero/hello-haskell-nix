{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Repository.Users.Statements (
    addUser,
    getUser,
    getUserByEmail
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

-- useTPGDatabase settings

getUser :: PGConnection -> UUID -> IO [(UUID, Text, Text)]
getUser conn user_id =
    pgQuery conn [pgSQL|
        SELECT      user_id,
                    name,
                    email
        FROM        hello.users
        WHERE user_id = ${user_id}
    |]

getUserByEmail :: PGConnection -> Text -> IO [(UUID, Text, Text)]
getUserByEmail conn email =
    pgQuery conn [pgSQL|
        SELECT      user_id,
                    email,
                    password
        FROM        hello.users
        WHERE email = ${email}
    |]

addUser :: PGConnection -> Text -> Text -> Text -> IO [UUID]
addUser conn name email password =
    pgQuery conn [pgSQL|
        INSERT INTO hello.users (name, email, password)
        VALUES (${name}, ${email}, ${password})
        RETURNING user_id
    |]
