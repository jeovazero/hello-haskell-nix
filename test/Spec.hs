{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
import App (app)
import Data.ByteString.Lazy (ByteString)
import Lib.Database (settings, withDatabase)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

newUser :: String -> String -> ByteString
newUser name email = [json|{
  name: #{name},
  password: "1234453",
  email: #{email}
}|]

main :: IO ()
main = withDatabase settings $ \conn -> hspec $ with (pure $ app conn) $ do
  describe "GET /" $ do
    it "responds with 200 / 'Hello Web'" $ do
      get "/" `shouldRespondWith` [json|{Hello: "Web"}|] {
        matchStatus = 200,
        matchHeaders = ["Content-Type" <:> "application/json"]
      }
  describe "POST /register" $ do
    it "responds with 200" $ do
      post "/register" (newUser "john" "john@dumb.com") `shouldRespondWith` 200

