{-# LANGUAGE OverloadedStrings #-}
module Users (usersHandler) where

import qualified Data.UUID as UUID
import Lib.Core
    ( Method(..)
    , jsonResponse
    , parseMethod
    , takeFirstPath
    , textResponseLBS
    )
import qualified Lib.Repository.Users.Data as D
import qualified Lib.Repository.Users.Handler as H
import Network.HTTP.Types (status200, status400, status405)
import Network.Wai (responseLBS, strictRequestBody)


usersHandler conn req respond =
    case parseMethod req of
      Post -> do
        maybeNewUser <- fmap D.decodeNewUser (strictRequestBody req)
        case maybeNewUser of
            Nothing -> textResponseLBS respond status400 "BAD"
            Just newUser -> do
                id <- H.addUser conn newUser
                textResponseLBS respond status200 $ UUID.toLazyASCIIBytes id

      _ -> respond $ responseLBS status405 [] ""
