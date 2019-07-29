{-# LANGUAGE OverloadedStrings #-}

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
    before_ (setupDb conn) $ do
      describe "/connect" $ do
        let queryParams = [("campaign_id", Just "1")]
        let postParams = [("CallUUID", "xxxxx"), ("From", "61411111111")]
        it "should give an intro and then redirect to the first call" $ do
          reqResponse <- handler $ Mocks.request "/connect" queryParams postParams
          reqResponse `shouldMatchBody` "<Speak>Welcome to the Test Campaign.</Speak>"
          reqResponse `shouldMatchBody` "<Redirect>https://apig.com/test/call</Redirect>"
        it "should create a caller record" $ do
          _ <- handler $ Mocks.request "/connect" queryParams postParams
          [(callerNumber, campaign_id, callUuid)] <-
            query_ conn "select number, campaign_id, call_uuid from callers limit 1" :: IO [(Text, Int, Text)]
          callerNumber `shouldBe` "61411111111"
          campaign_id `shouldBe` 1
          callUuid `shouldBe` "xxxxx"
      describe "/call" $
        it "should dial the target number" $ do
          let testData = ("1" :: String, "61400000000" :: String, "Test Target" :: String)
          _ <- execute conn "insert into targets (campaign_id, number, name) values (?, ?, ?)" testData
          reqResponse <- handler $ Mocks.request "/call" [] []
          reqResponse `shouldMatchBody` "<Speak>Calling the Test Target"
          reqResponse `shouldMatchBody`
            "<Dial action=\"https://apig.com/test/survey\" hangupOnStar=\"true\"><Number>61400000000"
      describe "/survey" $
        it "should announce that the call has ended and redirect TODO: ask survey" $ do
          reqResponse <- handler $ Mocks.request "/survey" [] []
          reqResponse `shouldMatchBody` "<Speak>The call has ended.</Speak>"
          reqResponse `shouldMatchBody` "<Redirect>https://apig.com/test/call</Redirect>"
      describe "/disconnect" $ do
        let callUuid = "xxx"
        let postParams = [("CallUUID", callUuid), ("Duration", "23")]
        it "returns the root path" $ handler (Mocks.request "/disconnect" [] postParams) `shouldReturn` xmlResponseOk
        it "should complete an existing callers record" $ do
          _ <-
            execute
              conn
              "insert into callers (call_uuid, campaign_id, number, created_at) values (?, 1, '0400000000', now())"
              [callUuid]
          _ <- handler $ Mocks.request "/disconnect" [] postParams
          [Only duration] <-
            (query conn "select duration from callers where call_uuid = ? and ended_at is not null limit 1" [callUuid]) :: IO [Only Int]
          duration `shouldBe` 23

setupDb :: Connection -> IO ()
setupDb conn = do
  flushDb conn
  let campaign = ("active" :: String, "Test Campaign" :: String, "Test instructions" :: String)
  _ <- execute conn "insert into campaigns (status, name, instructions) values (?, ?, ?)" campaign
  return ()

flushDb :: Connection -> IO ()
flushDb conn = do
  _ <- execute_ conn "truncate targets, calls, callers, campaigns RESTART IDENTITY"
  return ()
