{-# LANGUAGE OverloadedStrings #-}
module Auth (authRouter) where

import Network.HTTP.Types (status200, status400, status401, status405)
import Lib.Utils (
    Method(..),
    jsonResponse,
    textResponse,
    takeFirstPath,
    parseMethod)
import Lib.Repository.Users.Data (decodeUserCredentials, UserCredentials(..))
import qualified Lib.Repository.Users.Handler as H
import qualified Data.UUID as UUID
import Network.Wai (responseLBS, strictRequestBody)

authRouter conn req respond =
    case parseMethod req of
      Post -> do
        maybeCredentials <- fmap decodeUserCredentials (strictRequestBody req)
        case maybeCredentials of
            Nothing -> textResponse respond status400 "BAD"
            Just credentials -> do
                isAuthenticated <- H.verifyUserCredentials conn credentials
                case isAuthenticated of
                    False -> textResponse respond status401 "unauthorized"
                    True -> textResponse respond status200 "OK"

      _ -> respond $ responseLBS status405 [] ""
