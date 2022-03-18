{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib.Domain.User (
    addUser,
    verifyUserCredentials
) where
import qualified Control.Exception as C
import Data.Password.Argon2 as Argon
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Typed (PGConnection, pgTransaction)
import Lib.Data.User as UD (NewUser(..),User(..),UserCredentials(..)) 
import qualified Lib.Persistence.User.PGInstance as UPI
import qualified Lib.Persistence.User as UP
import Lib.Persistence
import Lib.Exception
import Data.Maybe

{-
data UserMessage
    = AddUser NewUser
    | VerifyCredentials UserCredentials

data UserResult
    = VerifiedCredentials (Maybe UUID)
    | AddedUser UUID
    | 

-}
argonHash password = do
    -- using the default parameters
    -- http://hackage.haskell.org/package/password-3.0.0.0/docs/src/Data.Password.Argon2.html#defaultParams
    hash <- Argon.hashPassword $ Argon.mkPassword password
    pure $ Argon.unPasswordHash hash

addUser :: (UP.UserPersistence a) => PersistenceConfig a -> NewUser -> IO UUID
addUser config newUser = do
    hash <- argonHash (nPassword newUser)    
    let newUser' = newUser{nPassword=hash}
    UPI.addUser config newUser'


verifyUserCredentials :: (UP.UserPersistence a) => PersistenceConfig a -> UserCredentials -> IO (Maybe UUID)
verifyUserCredentials config UserCredentials{cEmail,cPassword = rawPassword} = do
    mUser <- UPI.getUserByEmail config cEmail
    pure $ case mUser of
        Nothing -> Nothing
        Just User{userId,password=hashPassword} ->
            let
                argonPassword = Argon.mkPassword rawPassword
            in
            case Argon.checkPassword argonPassword (Argon.PasswordHash hashPassword) of
                Argon.PasswordCheckFail    -> Nothing
                Argon.PasswordCheckSuccess -> Just userId
