{-# LANGUAGE OverloadedStrings #-}
module Auth (authRouter, jwtMiddleware) where

import Network.HTTP.Types (hAuthorization, status200, status400, status401, status405)
import Lib.Utils (
    Method(..),
    jsonResponse,
    textResponse,
    takeFirstPath,
    parseMethod)
import Data.Aeson (Value(String))
import Lib.Repository.Users.Data (decodeUserCredentials, UserCredentials(..))
import qualified Lib.Repository.Users.Handler as H
import qualified Lib.JWT as JWT
import qualified Data.UUID as UUID
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Vault.Lazy as V
import Data.Word8 (isSpace, toLower)
import qualified Data.ByteString as BS
import Network.Wai (vault, Application, requestHeaders, Middleware, Request, Response, responseLBS, strictRequestBody)
import Lib.Database (PGConnection)
import Control.Monad (guard)
import qualified Data.Map as Map

-- 5 minutes in seconds
expirationTime = 5 * 60

authRouter :: PGConnection -> Text.Text -> Request -> (Response -> IO b) -> IO b
authRouter conn secret req respond =
    case parseMethod req of
      Post -> do
        maybeCredentials <- fmap decodeUserCredentials (strictRequestBody req)
        case maybeCredentials of
            Nothing -> textResponse respond status400 "BAD"
            Just credentials -> do
                userInfo <- H.verifyUserCredentials conn credentials

                case userInfo of
                    Nothing -> textResponse respond status401 "unauthorized"
                    Just id -> do
                        let payload = [("id", String $ UUID.toText id)]
                        token <- JWT.encode secret expirationTime payload
                        textResponse respond status200 (TextEnc.encodeUtf8 token)

      _ -> respond $ responseLBS status405 [] ""

jwtMiddleware :: Text.Text -> (V.Key (Map.Map Text.Text Value) -> Application) -> Application
jwtMiddleware secret app req respond =
    case lookup hAuthorization (requestHeaders req) of
        Nothing -> textResponse respond status401 "unauthorized"
        Just text -> do
            key <- V.newKey
            let (bearer, token) = BS.break isSpace text
            guard (BS.map toLower bearer == "bearer")
            let token' = BS.dropWhile isSpace token
            maybePayload <- JWT.decode secret (TextEnc.decodeUtf8 token')
            case maybePayload of
                Just payload -> do
                    print payload
                    let reqVault = V.insert key payload (vault req)
                    let req' = req { vault = reqVault }
                    app key req' respond
                Nothing -> do
                    textResponse respond status401 "unauthorized"