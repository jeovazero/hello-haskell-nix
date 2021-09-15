{-# LANGUAGE OverloadedStrings #-}
module App (app) where

import App.Auth (authHandler, jwtMiddleware)
import Lib.Layer.REST (fallbackRoute, jsonResponse, makeRoutes, (-->), (/*), (/~))
import Lib.Database (PGConnection)
import Network.HTTP.Types (status200)
import Network.Wai (Application)
import App.Register (registerHandler)
import Lib.Persistence (PersistenceConfig)
import Lib.Persistence.User as P
-- import App.Tools (toolsHandler)

helloWeb = "{\"Hello\":\"Web\"}"

secret = "dumb-secret"

-- TODO: add a custom exception handler
app :: (UserPersistence a) => PersistenceConfig a -> Application
app config
    = makeRoutes
        [ (/~) "register" --> registerHandler config
--        , (/~) "tools"    --> jwtMiddleware secret (toolsHandler conn)
        , (/~) "auth"     --> authHandler config secret
        , (/*)            --> \_ respond -> jsonResponse respond status200 helloWeb
        ]
