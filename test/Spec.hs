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
    describe "/connect" $ do
      it "should give an intro and then redirect to the first call" $ do
        reqResponse <- handler $ Mocks.request "/connect"
        reqResponse `shouldMatchBody`
          "<Speak>Welcome to the Test Campaign.</Speak>"
        reqResponse `shouldMatchBody`
          "<Redirect>http://localhost/call</Redirect>"
    describe "/call" $ do
      it "should dial the target number" $ do
        reqResponse <- handler $ Mocks.request "/call"
        reqResponse `shouldMatchBody`
          "<Speak>Calling the Sydney office.</Speak>"
        reqResponse `shouldMatchBody`
          "<Dial action=\"http://localhost/survey\" hangupOnStart=\"true\"><Number>61285994347</Number></Dial>"
    describe "/survey" $ do
      it "should announce that the call has ended and redirect TODO: ask survey" $ do
        reqResponse <- handler $ Mocks.request "/survey"
        reqResponse `shouldMatchBody` "<Speak>The call has ended.</Speak>"
        reqResponse `shouldMatchBody`
          "<Redirect>http://localhost/call</Redirect>"
    describe "/disconnect" $ do
      it "returns the root path" $ do
        handler (Mocks.request "/disconnect") `shouldReturn` xmlResponseOk
