{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
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
            Just UserCredentials{ email } -> do
                user <- H.getUserByEmail conn email
                case user of
                    Nothing -> textResponse respond status401 "unauthorized"
                    Just _ ->
                        textResponse respond status200 "OK"

      _ -> respond $ responseLBS status405 [] ""
