{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
 
module Lib.Repository.Users.Statements (
    addUser,
    getUser,
    getUserByEmail
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

getUser :: PGConnection -> UUID -> IO [(UUID, Text, Text)]
getUser conn user_id =
    pgQuery conn [pgSQL|
        SELECT      user_id,
                    name,
                    email
        FROM        hello.users
        WHERE user_id = ${user_id}
    |]

getUserByEmail :: PGConnection -> Text -> IO [(Text, Text)]
getUserByEmail conn email =
    pgQuery conn [pgSQL|
        SELECT      email,
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
