{-# LANGUAGE OverloadedStrings #-}
module Lib.Utils (
    jsonResponse,
    textResponse,
    takeFirstPath,
    Method(..),
    parseMethod) where

import Data.Text (Text)
import Network.Wai (
    Application,
    Request,
    responseLBS,
    pathInfo,
    requestMethod)

takeFirstPath req = _takeFirst $ pathInfo req
    where _takeFirst []              = Nothing
          _takeFirst (path:newPath)  = Just (path, req { pathInfo = newPath })

jsonResponse respond status content =
  respond $ responseLBS
            status
            [("Content-Type", "application/json")]
            content

textResponse respond status content =
  respond $ responseLBS
            status
            [("Content-Type", "text/plain")]
            content

data Method = GetMany
            | GetOne Text
            | Post
            | Put (Maybe Text)
            | Delete (Maybe Text)
            | NotImplemented

parseMethod :: Request -> Method
parseMethod req = case requestMethod req of
    "GET"    -> maybe GetMany GetOne maybeParam
    "POST"   -> Post
    "PUT"    -> Put maybeParam
    "DELETE" -> Delete maybeParam
    _        -> NotImplemented

    where maybeParam = fst <$> takeFirstPath req
