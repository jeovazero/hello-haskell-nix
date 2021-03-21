{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (ResponseReceived, 
    responseLBS,
    strictRequestBody,
    Request,
    Response)
import Lib.Utils (
    Method(..),
    jsonResponse,
    textResponse,
    takeFirstPath,
    parseMethod)
import qualified Data.UUID as UUID
import Network.HTTP.Types (status200, status204, status405, status404, status400)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Lib.Database (settings, withDatabase)
import qualified Lib.Repository.Tools.Handler as H
import qualified Lib.Repository.Tools.Data as D
import Tools (toolsRouter)
import Users (usersRouter)
import Auth (authRouter, jwtMiddleware)

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