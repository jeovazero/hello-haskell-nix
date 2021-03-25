{-# LANGUAGE OverloadedStrings #-}
module Lib.JWT (decode, encode) where

import Control.Monad (guard)
import Data.Aeson (Value)
import Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Prelude hiding (exp)
import Web.JWT hiding (decode)

headerJwt :: JOSEHeader
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

decode :: Text -> Text -> IO (Maybe (Map Text Value))
decode secret token = do
  let jwt = decodeAndVerifySignature (hmacSecret secret) token
  time <- getPOSIXTime
  pure $ case jwt of
    Nothing -> Nothing
    Just verifiedJwt -> do
      let claimsSet = claims verifiedJwt
      let maybeExpiration = exp claimsSet
      case maybeExpiration of
        Nothing -> Nothing
        Just expiration -> do
          guard (time <= secondsSinceEpoch expiration)
          Just (unClaimsMap $ unregisteredClaims claimsSet)
