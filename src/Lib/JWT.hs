{-# LANGUAGE OverloadedStrings #-}
module Lib.JWT (verify, encode) where

import Prelude hiding (exp)
import Web.JWT
import Data.Text (Text)
import Data.Aeson (Value)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Map as Map

headerJwt = JOSEHeader {
  alg = Just HS256,
  typ = Just "JWT",
  cty = Nothing,
  kid = Nothing
}

encode :: Text -> Int -> [(Text, Value)] -> IO Text
encode secret expirationTime payload = do
  time <- getPOSIXTime
  let expiration = time + fromIntegral expirationTime
  let key = hmacSecret secret
  let claimsSet = mempty {
    exp = numericDate expiration
    , unregisteredClaims = ClaimsMap (Map.fromList payload)
  }
  pure $ encodeSigned key headerJwt claimsSet