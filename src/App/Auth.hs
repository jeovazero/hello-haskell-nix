{-# LANGUAGE OverloadedStrings #-}
module App.Auth (authHandler, jwtMiddleware) where

import Control.Monad (guard)
import Data.Aeson (Value(String))
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.UUID as UUID
import qualified Data.Vault.Lazy as V
import Data.Word8 (isSpace, toLower)
import Lib.Core
    ( AppResult(..)
    , Method(..)
    , appResponse
    , jsonResponse
    , parseMethod
    , takeFirstPath
    , textResponse
    )
import Lib.Database (PGConnection)
import qualified Lib.JWT as JWT
import Lib.Repository.Users.Data (UserCredentials(..), decodeUserCredentials)
import qualified Lib.Repository.Users.Handler as H
import Network.HTTP.Types
    ( hAuthorization
    , status200
    , status400
    , status401
    , status405
    )
import Network.Wai
    ( Application
    , Middleware
    , Request
    , Response
    , requestHeaders
    , responseLBS
    , strictRequestBody
    , vault
    )

-- 5 minutes in seconds
expirationTime = 5 * 60

authHandler :: PGConnection -> T.Text -> Request -> (Response -> IO b) -> IO b
authHandler conn secret req respond =
    case parseMethod req of
      Post -> do
        maybeCredentials <- fmap decodeUserCredentials (strictRequestBody req)
        case maybeCredentials of
            Nothing -> appResponse respond BadRequest
            Just credentials -> do
                userInfo <- H.verifyUserCredentials conn credentials

                case userInfo of
                    Nothing -> appResponse respond Unauthorized
                    Just id -> do
                        let payload = [("id", String $ UUID.toText id)]
                        token <- JWT.encode secret expirationTime payload
                        let lToken = TLE.encodeUtf8 $ TL.fromStrict token
                        appResponse respond $ Ok lToken

      _ -> respond $ responseLBS status405 [] ""

jwtMiddleware :: T.Text -> (V.Key (Map.Map T.Text Value) -> Application) -> Application
jwtMiddleware secret app req respond =
    case lookup hAuthorization (requestHeaders req) of
        Nothing -> appResponse respond Unauthorized
        Just text -> do
            key <- V.newKey
            let (bearer, token) = BS.break isSpace text
            guard (BS.map toLower bearer == "bearer")
            let token' = BS.dropWhile isSpace token
            maybePayload <- JWT.decode secret (TE.decodeUtf8 token')
            case maybePayload of
                Just payload -> do
                    print payload
                    let reqVault = V.insert key payload (vault req)
                    let req' = req { vault = reqVault }
                    app key req' respond
                Nothing -> do
                    appResponse respond Unauthorized
