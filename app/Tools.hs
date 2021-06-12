{-# LANGUAGE OverloadedStrings #-}
module Tools (toolsHandler) where

import Control.Exception (Exception)
import Control.Monad (guard)
import Data.Aeson (Value(String))
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vault.Lazy as V
import Lib.Core (AppResult(..), Method(..), appResponse, parseMethod)
import Lib.Database (PGConnection)
import Lib.Exception (   AppException(..)
    , AppExceptionType(..)
    , DatabaseErrorType(..), appCatch)
import qualified Lib.Repository.Tools.Data as D
import qualified Lib.Repository.Tools.Handler as H
import Network.HTTP.Types
    ( status200
    , status204
    , status400
    , status401
    , status404
    , status405
    )
import Network.HTTP.Types.Status (Status)
import Network.Wai (Request, Response, responseLBS, strictRequestBody, vault)

uuidFromString :: Value -> Maybe UUID
uuidFromString (String id) = UUID.fromText id

userIdFromReq key req =
    V.lookup key (vault req)
        >>= Map.lookup "id"
        >>= uuidFromString

-- The tools router
toolsHandler :: PGConnection -> V.Key (Map.Map Text Value) -> Request -> (Response -> IO b) -> IO b
toolsHandler conn key req respond = do
    case userIdFromReq key req of
        Nothing -> appResponse respond Unauthorized
        Just userId -> do
            appResult <- appCatch $ toolsResolver conn userId req
            case appResult of
                Right result -> appResponse respond result
                Left err     -> appResponse respond $ decodeError err

decodeError :: AppException -> AppResult
decodeError (AppException err) =
    case err of
        Unexpected raw -> InternalServerError
        DatabaseError dbError ->
            case dbError of
                Duplicated -> Conflict "The specified tool already exists"
                Raw msg -> InternalServerError

toolsResolver :: PGConnection -> UUID -> Request -> IO AppResult
toolsResolver conn userId req = do
    let method = parseMethod req
    case method of
        GetMany -> do
            tools <- fmap D.encodeTools (H.getTools conn userId)
            pure $ Ok tools

        GetOne id ->
            case UUID.fromText id of
                Nothing -> pure BadRequest
                Just id -> do
                    tool <- H.getTool conn userId id
                    let toolDecoded = fmap D.encodeTool tool
                    case toolDecoded of
                        Nothing   -> pure $ NotFound
                        Just tool -> pure $ Ok tool

        Post -> do
            -- TODO: don't use strictRequestBody
            maybeNewTool <- fmap D.decodeNewTool (strictRequestBody req)
            case maybeNewTool of
                Nothing -> pure BadRequest
                Just newTool -> do
                    id <- H.addTool conn userId newTool
                    -- TODO: create a better message
                    pure $ Created (UUID.toLazyASCIIBytes id)

        -- TODO: refactor
        Put (Just id) -> do
            maybeUpdateTool <- fmap D.decodeUpdateTool (strictRequestBody req)
            print maybeUpdateTool
            case maybeUpdateTool of
                Nothing -> pure BadRequest
                Just updateTool -> do
                    case UUID.fromText id of
                        Nothing -> pure BadRequest
                        Just id -> do
                            maybeTool <- H.getTool conn userId id
                            case maybeTool of
                                Nothing -> pure BadRequest
                                Just tool -> do
                                    let (tagsToAdd, tagsToRemove, payload) = D.getUpdateInfo tool updateTool
                                    H.updateTool conn userId payload tagsToAdd tagsToRemove
                                    -- TODO: create a better message
                                    pure $ Ok "tool updated"

        Put Nothing -> pure BadRequest

        Delete (Just id) ->
            case UUID.fromText id of
            Nothing -> pure BadRequest
            Just id -> do
                H.removeToolById conn userId id
                pure NoContent

        Delete Nothing -> pure BadRequest

        _ -> pure MethodNotAllowed
