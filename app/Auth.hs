{-# LANGUAGE OverloadedStrings #-}
module Auth (authRouter) where

import Network.HTTP.Types (status200, status400, status401, status405)
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
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as TextEnc
import Network.Wai (responseLBS, strictRequestBody)

secret = "dumb-secret"

-- 5 minutes in seconds
expirationTime = 5 * 60

authRouter conn req respond =
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
                        let lazyToken = TextEnc.encodeUtf8 $ Text.fromStrict token
                        textResponse respond status200 lazyToken

      _ -> respond $ responseLBS status405 [] ""
