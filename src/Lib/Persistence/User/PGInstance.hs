{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Lib.Persistence.User.PGInstance (addUser,getUserByEmail) where

import Lib.Persistence
import Lib.Persistence.User
import qualified Lib.Persistence.User.PGStatements as PG
import Lib.Data.User (NewUser(..),User(..))
import qualified Control.Exception as C
import Data.Text (Text)
import Database.PostgreSQL.Typed (PGConnection,pgTransaction)

headOrThrow :: [a] -> String -> IO a
headOrThrow [] msg  = C.throwIO $ C.ErrorCall msg
headOrThrow (x:_) _ = pure x

safeHead []    = Nothing
safeHead (x:_) = Just x

instance UserPersistence PGConnection where
  getUserByEmail config email = pgTransaction conn $ do
    credentialsRaw <- PG.getUserByEmail conn email
    let maybeUserInfo = safeHead credentialsRaw
    pure $ fmap (\(userId, name, email, password) -> User{..}) maybeUserInfo
    where
      conn = pConfig config

  addUser config NewUser{nName,nEmail,nPassword} = pgTransaction conn $ do
    userUUID <- PG.addUser conn nName nEmail nPassword

    -- the Postgres Error should throw first
    headOrThrow userUUID "Something wrong when adding a new user"
    where
      conn = pConfig config