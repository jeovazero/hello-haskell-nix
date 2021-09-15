{-# LANGUAGE OverloadedStrings #-}
module Main where

import App (app)
import Lib.Database (settings, withDatabase)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Lib.Persistence (PersistenceConfig(..))

-- TODO: add more OWASP recommendation
-- TODO: add middleware to verify the body size
main :: IO ()
main = do
    putStrLn "http://localhost:8080/"
    withDatabase settings $ \conn ->
        run 8080 $ addHeaders [("Server", "")]  -- OWASP recommendation
                 $ logStdoutDev                 -- Logger
                 $ app PersistenceConfig{pConfig = conn}
