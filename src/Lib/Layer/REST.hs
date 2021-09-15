{-# LANGUAGE OverloadedStrings #-}
module Lib.Layer.REST (
    jsonResponse,
    textResponseLBS,
    textResponse,
    takeFirstPath,
    RESTMessage(..),
    parseMethod,
    ContentType(..),
    RESTResult(..),
    restResponse,
    makeRoutes,
    (/*),
    (/~),
    (-->),
    routeTo,
    fallbackRoute) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import Data.Text (Text)
import qualified Data.Text.Encoding as TEnc
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLEnc
import Lib.Exception (AppException)
import Network.HTTP.Types.Status
    ( Status
    , status200
    , status201
    , status204
    , status400
    , status401
    , status404
    , status405
    , status409
    , status500
    , status501
    )
import Network.Wai
    ( Application
    , Request
    , Response
    , pathInfo
    , requestMethod
    , responseLBS
    )

data ContentType = PlainText | JSON deriving (Eq, Show)

data RESTResponse = RESTResponse Status ContentType ByteString

data RESTResult
    = Ok ByteString
    | BadRequest
    | NotFound
    | NoContent
    | Conflict ByteString
    | Created ByteString
    | MethodNotAllowed
    | NotImplemented
    | InternalServerError
    | Unauthorized
    | Exceptional AppException ByteString
    deriving (Eq, Show)

errorMessage :: ByteString -> ByteString
errorMessage message =
    encode $ object ["message" .= TEnc.decodeUtf8 (toStrict message)]

decodeRESTResponse :: RESTResult -> RESTResponse
decodeRESTResponse restResponse =
    case restResponse of
        Ok content ->
            RESTResponse status200 JSON content
        Created content ->
            RESTResponse status201 PlainText content
        NoContent ->
            RESTResponse status204 PlainText ""
        BadRequest ->
            RESTResponse status400 JSON (errorMessage "Bad Request")
        Unauthorized ->
            RESTResponse status401 JSON (errorMessage "Unauthorized")
        NotFound ->
            RESTResponse status404 JSON (errorMessage "Not Found")
        MethodNotAllowed ->
            RESTResponse status405 JSON (errorMessage "Method Not Allowed")
        Conflict msg ->
            RESTResponse status409 JSON msg
        NotImplemented ->
            RESTResponse status501 JSON (errorMessage "Not Implemented")
        Exceptional err msg ->
            -- TODO: decode the exceptional situations
            RESTResponse status500 JSON (errorMessage msg)
        _ ->
            RESTResponse status500 JSON (errorMessage "Internal Server Error")

restResponse :: (Response -> t) -> RESTResult -> t
restResponse respond response = do
    let (RESTResponse status contentType message) = decodeRESTResponse response
    let contentType' | contentType == JSON = "application/json"
                     | otherwise = "text/plain"
    respond $ responseLBS
              status
              [("Content-Type", contentType')]
              message

takeFirstPath :: Request -> Maybe (Text, Request)
takeFirstPath req = _takeFirst $ pathInfo req
    where _takeFirst []             = Nothing
          _takeFirst (path:newPath) = Just (path, req { pathInfo = newPath })

jsonResponse :: (Response -> t) -> Status -> ByteString -> t
jsonResponse respond status content =
  respond $ responseLBS
            status
            [("Content-Type", "application/json")]
            content

textResponseLBS :: (Response -> t) -> Status -> ByteString -> t
textResponseLBS respond status content =
  respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            content

textResponse respond status content = do
    let contentLazy = fromStrict content
    textResponseLBS respond status contentLazy

data RESTMessage
  = GetOne Text
  | GetMany
  | Post
  | Put (Maybe Text)
  | Delete (Maybe Text)
  | NotAllowed

parseMethod :: Request -> RESTMessage
parseMethod req = case requestMethod req of
  "GET"    -> maybe GetMany GetOne maybeParam
  "POST"   -> Post
  "PUT"    -> Put maybeParam
  "DELETE" -> Delete maybeParam
  _        -> NotAllowed

  where maybeParam = fmap fst $ takeFirstPath req

(/~), routeTo :: p -> Application -> (Maybe p, Application)
(/~) p app = (Just p, app)

infixr 0 /~

routeTo = (/~)

(-->) = ($)

fallbackRoute :: Application -> (Maybe a, Application)
fallbackRoute app = (Nothing, app)

(/*) :: Application -> (Maybe a, Application)
(/*) = fallbackRoute

makeRoutes :: [(Maybe Text, Application)] -> Application
makeRoutes routes req respond =
  case pathInfo req of
    (path:rest) ->
      let
          option = matchRoute path
      in
      case option of
          Nothing  -> restResponse respond NotFound
          Just app -> app (req { pathInfo = rest }) respond
    [] ->
      let
          option = matchRoute ""
      in
      case option of
          Nothing  -> restResponse respond NotFound
          Just app -> app req respond
  where
    matchRoute path = foldl (handle path) Nothing routes

    handle _ (Just app) _ = Just app
    handle _ _ (Nothing, app) = Just app
    handle path _ (Just routeName, app)
      | routeName == path = Just app
      | otherwise = Nothing
