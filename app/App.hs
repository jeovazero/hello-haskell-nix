{-# LANGUAGE OverloadedStrings #-}
module App (app) where

import Auth (authHandler, jwtMiddleware)
import Lib.Core (fallbackRoute, jsonResponse, makeRoutes, (-->), (/*), (/~))
import Lib.Database (PGConnection)
import Network.HTTP.Types (status200)
import Network.Wai (Application)
import Register (registerHandler)
import Tools (toolsHandler)

helloWeb = "{\"Hello\":\"Web\"}"

secret = "dumb-secret"

-- TODO: add a custom exception handler
app :: PGConnection -> Application
app conn
    = makeRoutes
        [ (/~) "register" --> registerHandler conn
        , (/~) "tools"    --> jwtMiddleware secret (toolsHandler conn)
        , (/~) "auth"     --> authHandler conn secret
        , (/*)            --> \_ respond -> jsonResponse respond status200 helloWeb
        ]
