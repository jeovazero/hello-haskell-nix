{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Persistence.User.PGStatements (
    addUser,
    getUserByEmail
) where

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Typed (PGConnection, pgQuery, pgSQL)
import Database.PostgreSQL.Typed.Array
import Database.PostgreSQL.Typed.Query (PGSimpleQuery)
import Database.PostgreSQL.Typed.Types
import Prelude hiding (words)

getUserByEmail :: PGConnection -> Text -> IO [(UUID, Text, Text, Text)]
getUserByEmail conn email =
    pgQuery conn [pgSQL|
        SELECT      user_id,
                    name,
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
