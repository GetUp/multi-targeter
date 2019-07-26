{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import AWSLambda.Events.APIGateway
import Data.Aeson.TextValue
import Data.Text
import Database.PostgreSQL.Simple
import Test.Hspec

import Handler
import Mocks

shouldMatchBody :: APIGatewayProxyResponse Text -> Text -> Expectation
shouldMatchBody (APIGatewayProxyResponse _ _ (Just (TextValue body))) fragment =
  unpack body `shouldContain` unpack fragment
shouldMatchBody _ _ = False `shouldBe` True

main :: IO ()
main = do
  url <- dbUrl
  conn <- connectPostgreSQL url
  hspec $
    before_ (flushDb conn) $ do
      describe "/connect" $ do
        it "should give an intro and then redirect to the first call" $ do
          reqResponse <- handler $ Mocks.request "/connect"
          reqResponse `shouldMatchBody` "<Speak>Welcome to the Test Campaign.</Speak>"
          reqResponse `shouldMatchBody` "<Redirect>https://apig.com/test/call</Redirect>"
      describe "/call" $ do
        it "should dial the target number" $ do
          let testData = ("61400000000" :: String, "Test Target" :: String)
          _ <- execute conn "insert into targets (number, name) values (?, ?)" testData
          reqResponse <- handler $ Mocks.request "/call"
          reqResponse `shouldMatchBody` "<Speak>Calling the Test Target"
          reqResponse `shouldMatchBody`
            "<Dial action=\"https://apig.com/test/survey\" hangupOnStar=\"true\"><Number>61400000000"
      describe "/survey" $ do
        it "should announce that the call has ended and redirect TODO: ask survey" $ do
          reqResponse <- handler $ Mocks.request "/survey"
          reqResponse `shouldMatchBody` "<Speak>The call has ended.</Speak>"
          reqResponse `shouldMatchBody` "<Redirect>https://apig.com/test/call</Redirect>"
      describe "/disconnect" $ do
        it "returns the root path" $ do handler (Mocks.request "/disconnect") `shouldReturn` xmlResponseOk

flushDb :: Connection -> IO ()
flushDb conn = do
  _ <- execute_ conn "truncate targets"
  return ()
