{-# LANGUAGE OverloadedStrings #-}
module Lib.Utils (
    jsonResponse,
    textResponseLBS,
    textResponse,
    takeFirstPath,
    Method(..),
    parseMethod,
    ContentType(..),
    AppResult(..),
    appResponse) where

import Data.Text (Text)
import Network.Wai (
    Response, 
    Application,
    Request,
    responseLBS,
    pathInfo,
    requestMethod)
import Data.ByteString.Lazy (toStrict, fromStrict, ByteString)
import Network.HTTP.Types.Status (status500, status501, status204, status201, status405, status404, status400, status200, status401, Status)
import Data.Aeson (object, encode, (.=))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TEnc
import qualified Data.Text.Lazy.Encoding as TLEnc
import Lib.Exception (AppException)

data ContentType = PlainText | JSON deriving (Eq, Show)
data AppResponse = AppResponse Status ContentType ByteString
data AppResult
    = Ok ByteString
    | BadRequest
    | NotFound
    | NoContent
    | Created ByteString
    | MethodNotAllowed
    | NotImplemented
    | InternalServerError
    | Unauthorized
    | Exceptional AppException
    deriving (Eq, Show)

errorMessage :: ByteString -> ByteString
errorMessage message =
    encode $ object ["message" .= TEnc.decodeUtf8 (toStrict message)]

decodeAppResponse :: AppResult -> AppResponse
decodeAppResponse appResponse =
    case appResponse of
        Ok content -> 
            AppResponse status200 JSON content
        Created content -> 
            AppResponse status201 PlainText content
        NoContent -> 
            AppResponse status204 PlainText ""
        BadRequest ->
            AppResponse status400 JSON (errorMessage "Bad Request")
        Unauthorized -> 
            AppResponse status401 JSON (errorMessage "Unauthorized")
        NotFound ->
            AppResponse status404 JSON (errorMessage "Not Found")
        MethodNotAllowed ->
            AppResponse status405 JSON (errorMessage "Method Not Allowed")
        NotImplemented ->
            AppResponse status501 JSON (errorMessage "Not Implemented")
        Exceptional err ->
            -- TODO: decode the exceptional situations
            AppResponse status500 JSON (errorMessage (TLEnc.encodeUtf8 . TL.pack $ show err))
        _ ->
            AppResponse status500 JSON (errorMessage "Internal Server Error")

appResponse respond response = do
    let (AppResponse status contentType message) = decodeAppResponse response
    let contentType' | contentType == JSON = "application/json"
                     | otherwise = "text/plain"
    respond $ responseLBS
              status
              [("Content-Type", contentType')]
              message

takeFirstPath req = _takeFirst $ pathInfo req
    where _takeFirst []              = Nothing
          _takeFirst (path:newPath)  = Just (path, req { pathInfo = newPath })

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

data Method = GetMany
            | GetOne Text
            | Post
            | Put (Maybe Text)
            | Delete (Maybe Text)
            | NotAllowed

parseMethod :: Request -> Method
parseMethod req = case requestMethod req of
    "GET"    -> maybe GetMany GetOne maybeParam
    "POST"   -> Post
    "PUT"    -> Put maybeParam
    "DELETE" -> Delete maybeParam
    _        -> NotAllowed

    where maybeParam = fst <$> takeFirstPath req
