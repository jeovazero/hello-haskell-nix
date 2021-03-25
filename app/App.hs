{-# LANGUAGE OverloadedStrings #-}
module App (app) where

import Auth (authHandler, jwtMiddleware)
import Lib.Core (fallbackRoute, jsonResponse, makeRoutes, (-->), (/*), (/~))
import Lib.Database (PGConnection)
import Network.HTTP.Types (status200)
import Network.Wai (Application)
import Tools (toolsHandler)
import Users (usersHandler)

helloWeb = "{\"Hello\":\"Web\"}"

secret = "dumb-secret"

-- TODO: add a custom exception handler
app :: PGConnection -> Application
app conn
    = makeRoutes
        [ (/~) "tools" --> jwtMiddleware secret (toolsHandler conn)
        , (/~) "users" --> usersHandler conn
        , (/~) "auth"  --> authHandler conn secret
        , (/*)         --> \_ respond -> jsonResponse respond status200 helloWeb
        ]
