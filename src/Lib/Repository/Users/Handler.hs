{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib.Repository.Users.Handler (
    addUser,
    getUserByEmail
) where
import qualified Lib.Repository.Users.Statements as S
import qualified Control.Exception as C
import Data.UUID (UUID)
import Data.Text (Text)
import Database.PostgreSQL.Typed (pgTransaction, PGConnection)
import Lib.Repository.Users.Data (NewUser(..), UserCredentials(..))

-- WTF
headOrThrow :: [a] -> IO a
headOrThrow [] = C.throwIO $ C.ErrorCall "empty list"
headOrThrow (x:_) = pure x

safeHead [] = Nothing
safeHead (x:_) = Just x

addUser :: PGConnection -> NewUser -> IO UUID
addUser conn NewUser{ name, email, password } = pgTransaction conn $ do
    userUUID' <- S.addUser conn name email password
    headOrThrow userUUID'

getUserByEmail :: PGConnection -> Text -> IO (Maybe UserCredentials)
getUserByEmail conn email = do
    credentials <- S.getUserByEmail conn email
    pure $ fmap (uncurry UserCredentials) $ safeHead credentials
