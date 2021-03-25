{-# LANGUAGE OverloadedStrings #-}
module Spec where

import App (app)
import Lib.Database (settings, withDatabase)
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = withDatabase settings $ \conn -> hspec $ with (pure $ app conn) $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
