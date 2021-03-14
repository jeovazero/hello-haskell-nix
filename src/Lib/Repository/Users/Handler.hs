{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib.Repository.Users.Handler (
    addUser,
    verifyUserCredentials
) where
import qualified Lib.Repository.Users.Statements as S
import qualified Control.Exception as C
import Data.UUID (UUID)
import Data.Text (Text)
import Database.PostgreSQL.Typed (pgTransaction, PGConnection)
import Lib.Repository.Users.Data (NewUser(..), UserCredentials(..))
import Data.Password.Argon2 as Argon

-- WTF
headOrThrow :: [a] -> IO a
headOrThrow [] = C.throwIO $ C.ErrorCall "empty list"
headOrThrow (x:_) = pure x

safeHead [] = Nothing
safeHead (x:_) = Just x

argonHash password = do
    -- using the default parameters
    -- http://hackage.haskell.org/package/password-3.0.0.0/docs/src/Data.Password.Argon2.html#defaultParams
    hash <- Argon.hashPassword $ Argon.mkPassword password
    pure $ Argon.unPasswordHash hash

addUser :: PGConnection -> NewUser -> IO UUID
addUser conn NewUser{ name, email, password } = pgTransaction conn $ do
    hash <- argonHash password
    userUUID' <- S.addUser conn name email hash
    headOrThrow userUUID'

getUserCredentialsByEmail :: PGConnection -> Text -> IO (Maybe UserCredentials)
getUserCredentialsByEmail conn email = do
    credentials <- S.getUserByEmail conn email
    pure (uncurry UserCredentials <$> safeHead credentials)

verifyUserCredentials conn UserCredentials{ email, password = rawPassword } = do
    let argonPassword = Argon.mkPassword rawPassword
    credentials <- getUserCredentialsByEmail conn email
    pure $ case credentials of
      Nothing -> False
      Just UserCredentials{ password = hashPassword } ->
          case Argon.checkPassword argonPassword (Argon.PasswordHash hashPassword) of
              Argon.PasswordCheckFail -> False
              Argon.PasswordCheckSuccess -> True

