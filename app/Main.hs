{-# LANGUAGE OverloadedStrings #-}

import Auth (authRouter, jwtMiddleware)
import qualified Data.UUID as UUID
import Lib.Database (settings, withDatabase)
import qualified Lib.Repository.Tools.Data as D
import qualified Lib.Repository.Tools.Handler as H
import Lib.Utils
    ( Method(..)
    , jsonResponse
    , parseMethod
    , takeFirstPath
    , textResponse
    )
import Network.HTTP.Types
    ( status200
    , status204
    , status400
    , status404
    , status405
    )
import Network.Wai
    ( Request
    , Response
    , ResponseReceived
    , responseLBS
    , strictRequestBody
    )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Tools (toolsRouter)
import Users (usersRouter)

helloWeb = "{\"Hello\":\"Web\"}"

secret = "dumb-secret"

-- TODO: add a custom exception handler
app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app req respond =
    withDatabase settings $ \conn ->
        case takeFirstPath req of
          Just ("tools", req') -> jwtMiddleware secret (toolsRouter conn) req' respond
          Just ("users", req') -> usersRouter conn req' respond
          Just ("auth", req') -> authRouter conn secret req' respond
          _                    -> jsonResponse respond status200 helloWeb


-- TODO: add more OWASP recommendation
-- TODO: add middleware to verify the body size
main :: IO ()
main = do
    putStrLn "http://localhost:8080/"

    run 8080 $ addHeaders [("Server", "")]  -- OWASP recommendation
             $ logStdoutDev                 -- Logger
             $ app
