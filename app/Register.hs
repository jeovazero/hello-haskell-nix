{-# LANGUAGE OverloadedStrings #-}
module Register (registerHandler) where

import qualified Data.ByteString
import qualified Data.UUID as UUID
import Lib.Core (AppResult(..), Method(..), appResponse, parseMethod)
import Lib.Database (PGConnection)
import Lib.Exception
    ( AppException(..)
    , AppExceptionType(..)
    , DatabaseErrorType(..)
    , appCatch
    )
import qualified Lib.Repository.Users.Data as D
import qualified Lib.Repository.Users.Handler as H
import Network.HTTP.Types (status200, status400, status405)
import Network.Wai (Application, Request, responseLBS, strictRequestBody)

registerHandler :: PGConnection -> Application
registerHandler conn req respond = do
    appResult <- appCatch $ register conn req
    case appResult of
        Right result -> appResponse respond result
        Left err     -> appResponse respond $ decodeError err

decodeError :: AppException -> AppResult
decodeError (AppException err) =
    case err of
        Unexpected raw -> InternalServerError
        DatabaseError dbError ->
            case dbError of
                Duplicated -> Conflict "The specified register already exists"
                Raw msg -> InternalServerError

register :: PGConnection -> Request -> IO AppResult
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
