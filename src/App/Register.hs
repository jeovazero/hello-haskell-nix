{-# LANGUAGE OverloadedStrings #-}
module App.Register (registerHandler) where

import qualified Data.ByteString
import qualified Data.UUID as UUID
import Lib.Layer.REST (RESTResult(..), RESTMessage(..), restResponse, parseMethod)
import Lib.Persistence (PersistenceConfig)
import Lib.Exception
    ( AppException(..)
    , AppExceptionType(..)
    , DatabaseErrorType(..)
    , appCatch
    )
import qualified Lib.Data.User as D
import qualified Lib.Domain.User as H
import Network.HTTP.Types (status200, status400, status405)
import Network.Wai (Application, Request, responseLBS, strictRequestBody)
import Lib.Persistence.User as P

registerHandler :: (UserPersistence a) => PersistenceConfig a -> Application
registerHandler conn req respond = do
    appResult <- appCatch $ register conn req
    case appResult of
        Right result -> restResponse respond result
        Left err     -> restResponse respond $ decodeError err

decodeError :: AppException -> RESTResult
decodeError (AppException err) =
    case err of
        Unexpected raw -> InternalServerError
        DatabaseError dbError ->
            case dbError of
                Duplicated -> Conflict "The specified register already exists"
                Raw msg -> InternalServerError

register :: (UserPersistence a) => PersistenceConfig a -> Request -> IO RESTResult
register conn req =
    case parseMethod req of
      Post -> do
        maybeNewUser <- fmap D.decodeNewUser (strictRequestBody req)
        case maybeNewUser of
            Nothing -> pure BadRequest
            Just newUser -> do
                id <- H.addUser conn newUser
                pure . Created $ UUID.toLazyASCIIBytes id

      _ -> pure MethodNotAllowed
