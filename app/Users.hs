{-# LANGUAGE OverloadedStrings #-}
module Users (usersRouter) where

import Network.HTTP.Types (status200, status400, status405)
import Lib.Utils (
    Method(..),
    jsonResponse,
    textResponse,
    takeFirstPath,
    parseMethod)
import qualified Lib.Repository.Users.Data as D
import qualified Lib.Repository.Users.Handler as H
import qualified Data.UUID as UUID
import Network.Wai (responseLBS, strictRequestBody)


usersRouter conn req respond =
    case parseMethod req of
      Post -> do
        maybeNewUser <- fmap D.decodeNewUser (strictRequestBody req)
        case maybeNewUser of
            Nothing -> textResponse respond status400 "BAD"
            Just newUser -> do
                id <- H.addUser conn newUser
                textResponse respond status200 $ UUID.toLazyASCIIBytes id

      _ -> respond $ responseLBS status405 [] ""
