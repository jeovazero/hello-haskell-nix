{-# LANGUAGE OverloadedStrings #-}
module Tools (toolsRouter) where

import qualified Data.UUID as UUID
import qualified Lib.Repository.Tools.Data as D
import qualified Lib.Repository.Tools.Handler as H
import Network.Wai (vault, 
    Response,
    Request, 
    responseLBS,
    strictRequestBody)
import Network.HTTP.Types (status401, status200, status204, status405, status404, status400)
import Lib.Utils (
    Method(..),
    jsonResponse,
    textResponseLBS,
    takeFirstPath,
    parseMethod)
import Lib.Database (PGConnection)
import qualified Data.Map as Map
import qualified Data.Vault.Lazy as V
import Data.Text
import Data.Aeson (Value(String))
import Data.Maybe (isJust)
import Control.Monad (guard)

uuidFromString (String id) = UUID.fromText id

userIdFromReq key req =
    V.lookup key (vault req)
        >>= Map.lookup "id"
        >>= uuidFromString

-- The tools router
toolsRouter :: PGConnection -> V.Key (Map.Map Text Value) -> Request -> (Response -> IO b) -> IO b
toolsRouter conn key req respond = do
    case userIdFromReq key req of
        Nothing -> respond $ responseLBS status401 [] "unauthorized"
        Just userId -> do
            case parseMethod req of
                GetMany -> do
                    tools <- fmap D.encodeTools (H.getTools conn userId)
                    jsonResponse respond status200 tools

                GetOne id ->
                    case UUID.fromText id of
                    Nothing -> textResponseLBS respond status400 "BAD"
                    Just id -> do
                        tool <- H.getTool conn userId id
                        let toolDecoded = fmap D.encodeTool tool

                        case toolDecoded of
                            Nothing -> textResponseLBS respond status404 "Not Found"
                            Just tool -> jsonResponse respond status200 tool

                Post -> do
                    print "post"
                    maybeNewTool <- fmap D.decodeNewTool (strictRequestBody req)
                    print maybeNewTool
                    case maybeNewTool of
                        Nothing -> textResponseLBS respond status400 "BAD"
                        Just newTool -> do
                            id <- H.addTool conn userId newTool
                            textResponseLBS respond status200 $ UUID.toLazyASCIIBytes id

                Put (Just id) -> do
                    print "put"
                    maybeUpdateTool <- fmap D.decodeUpdateTool (strictRequestBody req)
                    print maybeUpdateTool
                    case maybeUpdateTool of
                        Nothing -> textResponseLBS respond status400 "BAD"
                        Just updateTool -> do
                            case UUID.fromText id of
                                Nothing -> textResponseLBS respond status400 "BAD"
                                Just id -> do
                                    maybeTool <- H.getTool conn userId id
                                    case maybeTool of
                                        Just tool -> do
                                            let (tagsToAdd, tagsToRemove, payload) = D.getUpdateInfo tool updateTool
                                            print tagsToAdd
                                            print tagsToRemove
                                            H.updateTool conn userId payload tagsToAdd tagsToRemove
                                            textResponseLBS respond status200 "OK"
                                        Nothing -> textResponseLBS respond status400 "BAD"

                Put Nothing -> textResponseLBS respond status400 "BAD"

                Delete (Just id) -> 
                    case UUID.fromText id of
                    Nothing -> textResponseLBS respond status400 "BAD"
                    Just id -> do
                        H.removeToolById conn userId id
                        textResponseLBS respond status204 ""

                Delete Nothing -> textResponseLBS respond status400 "BAD"

                _ -> respond $ responseLBS status405 [] "Not Implemented"
