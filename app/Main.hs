{-# LANGUAGE OverloadedStrings #-}

import Network.Wai (
    responseLBS,
    strictRequestBody)
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

-- The tools router
toolsRouter conn req respond =
    case parseMethod req of
        GetMany -> do
            tools <- fmap D.encodeTools (H.getTools conn)
            jsonResponse respond status200 tools

        GetOne id ->
            case UUID.fromText id of
              Nothing -> textResponse respond status400 "BAD"
              Just id -> do
                  tool <- H.getTool conn id
                  let toolDecoded = fmap D.encodeTool tool

                  case toolDecoded of
                    Nothing -> textResponse respond status404 "Not Found"
                    Just tool -> jsonResponse respond status200 tool

        Post -> do
            print "post"
            maybeNewTool <- fmap D.decodeNewTool (strictRequestBody req)
            print maybeNewTool
            case maybeNewTool of
                Nothing -> textResponse respond status400 "BAD"
                Just newTool -> do
                    id <- H.addTool conn newTool
                    textResponse respond status200 $ UUID.toLazyASCIIBytes id

        Put (Just id) -> do
            print "put"
            maybeUpdateTool <- fmap D.decodeUpdateTool (strictRequestBody req)
            print maybeUpdateTool
            case maybeUpdateTool of
                Nothing -> textResponse respond status400 "BAD"
                Just updateTool -> do
                    case UUID.fromText id of
                        Nothing -> textResponse respond status400 "BAD"
                        Just id -> do
                            maybeTool <- H.getTool conn id
                            case maybeTool of
                              Just tool -> do
                                let (tagsToAdd, tagsToRemove, payload) = D.getUpdateInfo tool updateTool
                                print tagsToAdd
                                print tagsToRemove
                                H.updateTool conn payload tagsToAdd tagsToRemove
                                textResponse respond status200 "OK"
                              Nothing -> textResponse respond status400 "BAD"

        Put Nothing -> textResponse respond status400 "BAD"

        Delete (Just id) -> 
            case UUID.fromText id of
              Nothing -> textResponse respond status400 "BAD"
              Just id -> do
                  H.removeToolById conn id
                  textResponse respond status204 ""

        Delete Nothing -> textResponse respond status400 "BAD"

        _ -> respond $ responseLBS status405 [] ""



helloWeb = "{\"Hello\":\"Web\"}"

-- TODO: add a custom exception handler
app req respond = 
    withDatabase settings $ \conn ->
        case takeFirstPath req of
          Just ("tools", req') -> toolsRouter conn req' respond
          _                    -> jsonResponse respond status200 helloWeb


-- TODO: add more OWASP recommendation
-- TODO: add middleware to verify the body size
main = do
    putStrLn "http://localhost:8080/"

    run 8080 $ addHeaders [("Server", "")]  -- OWASP recommendation
             $ logStdoutDev                 -- Logger
             $ app
