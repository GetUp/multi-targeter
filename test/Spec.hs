{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           AWSLambda.Events.APIGateway
import           Data.Aeson.TextValue
import           Data.Text
import           Test.Hspec

import           Handler
import           Mocks

shouldMatchBody :: APIGatewayProxyResponse Text -> Text -> Expectation
shouldMatchBody (APIGatewayProxyResponse _ _ (Just (TextValue body))) fragment =
  unpack body `shouldContain` unpack fragment
shouldMatchBody _ _ = False `shouldBe` True

main :: IO ()
main =
  hspec $ do
    describe "Handler" $ do
      context "with an empty request" $ do
        it "returns hello world in xml" $ do
          reqResponse <- handler Mocks.request
          reqResponse `shouldMatchBody` "<Speak>Hello from haskell</Speak>"
