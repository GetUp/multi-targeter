{-# LANGUAGE OverloadedStrings #-}

import AWSLambda.Events.APIGateway
import Data.Aeson.TextValue
import qualified Data.ByteString.Internal as BS
import Data.Text
import Database.PostgreSQL.Simple
import Test.Hspec

import Handler
import Mocks

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
        before_ (callEndpointSetup conn 5) $ do
          let postParams = [("CallUUID", "xxxxx"), ("From", "61411111111")]
          it "should dial the target number" $ do
            reqResponse <- handler $ Mocks.request "/call" [] postParams
            [Only callId] <- query_ conn "select id from calls order by created_at desc limit 1" :: IO [Only Int]
            reqResponse `shouldMatchBody` "<Speak>Calling the Test Target"
            reqResponse `shouldMatchBody`
              ("<Dial action=\"https://apig.com/test/survey?call_id=" <> tShow callId <>
               "\" hangupOnStar=\"true\" timeLimit=\"1800\" timeout=\"30\"><Number>61400000000")
          it "should log the call" $ do
            _ <- handler $ Mocks.request "/call" [] postParams
            [(caller_id, target_id)] <- query_ conn "select caller_id, target_id from calls limit 1" :: IO [(Int, Int)]
            caller_id `shouldBe` 5
            target_id `shouldBe` 1
          context "when the target has already been called by the caller" $ do
            it "should tell the caller and redirect to thanks" $ do
              _ <- insertTestCall conn 5
              reqResponse <- handler $ Mocks.request "/call" [] postParams
              reqResponse `shouldMatchBody` "<Speak>All the targets have been called."
              reqResponse `shouldMatchBody` "<Redirect>https://apig.com/test/thanks</Redirect>"
          context "when the target has already been called by a caller with the same number" $ do
            it "should tell the caller and redirect to thanks" $ do
              _ <- insertTestCaller conn 9
              _ <- insertTestCall conn 9
              reqResponse <- handler $ Mocks.request "/call" [] postParams
              reqResponse `shouldMatchBody` "<Speak>All the targets have been called."
              reqResponse `shouldMatchBody` "<Redirect>https://apig.com/test/thanks</Redirect>"
          context "when the target is not active" $ do
            it "should tell the caller and redirect to thanks" $ do
              _ <- execute_ conn "update targets set active = false"
              reqResponse <- handler $ Mocks.request "/call" [] postParams
              reqResponse `shouldMatchBody` "<Speak>All the targets have been called."
              reqResponse `shouldMatchBody` "<Redirect>https://apig.com/test/thanks</Redirect>"
      describe "/survey" $ do
        let callerId = 15
        let postParams status =
              [ ("DialALegUUID", "xxxxx")
              , ("DialBLegUUID", "yyyyy")
              , ("DialHangupCause", "NORMAL_CLEARING")
              , ("DialStatus", status)
              ]
        before_ (surveyEndpointSetup conn callerId) $ do
          it "should update the call record" $ do
            [Only callId] <- insertTestCall conn callerId
            let queryParams = [("call_id", Just $ bShow callId)]
            _ <- handler $ Mocks.request "/survey" queryParams (postParams "completed")
            let callQuery =
                  query conn "select target_id, status, hangup_cause, call_uuid from calls where id = ?" [callId]
            [(target_id, status, hangup_cause, call_uuid)] <- callQuery :: IO [(Int, Text, Text, Text)]
            target_id `shouldBe` 1
            status `shouldBe` "completed"
            hangup_cause `shouldBe` "NORMAL_CLEARING"
            call_uuid `shouldBe` "yyyyy"
          context "when the call connected and completed normally" $ do
            let completedCallParams = postParams "completed"
            it "should say call has ended, ask for the outcome, then redirect if no input received" $ do
              [Only callId] <- insertTestCall conn callerId
              let queryParams = [("call_id", Just $ bShow callId)]
              reqResponse <- handler $ Mocks.request "/survey" queryParams completedCallParams
              reqResponse `shouldMatchBody` "<Speak>The call has ended."
              reqResponse `shouldMatchBody`
                ("<GetDigits action=\"https://apig.com/test/survey_response?call_id=" <> tShow callId)
              reqResponse `shouldMatchBody` "</GetDigits><Redirect>https://apig.com/test/thanks</Redirect>"
          context "when the call was not answered" $ do
            let busyCallParams = postParams "busy"
            it "should not announce anything, just redirect back to /call" $ do
              [Only callId] <- insertTestCall conn callerId
              let queryParams = [("call_id", Just $ bShow callId)]
              reqResponse <- handler $ Mocks.request "/survey" queryParams busyCallParams
              reqResponse `shouldNotMatchBody` "<Speak>The call has ended.</Speak>"
              reqResponse `shouldMatchBody` "<Redirect>https://apig.com/test/call</Redirect>"
      describe "/survey_response" $ do
        let callerId = 15
        let postParams digit = [("Digits", bShow digit)]
        before_ (surveyEndpointSetup conn callerId) $ do
          it "should record the call outcome" $ do
            [Only callId] <- insertTestCall conn callerId
            let queryParams = [("call_id", Just $ bShow callId)]
            _ <- handler $ Mocks.request "/survey_response" queryParams (postParams 1)
            [Only outcome] <- query conn "select outcome from calls where id = ?" [callId] :: IO [Only Text]
            outcome `shouldBe` "conversation"
          it "should allow 1 (and only 1) to be pressed to call again then redirect to /thanks" $ do
            [Only callId] <- insertTestCall conn callerId
            let queryParams = [("call_id", Just $ bShow callId)]
            reqResponse <- handler $ Mocks.request "/survey_response" queryParams (postParams 1)
            reqResponse `shouldMatchBody` "<GetDigits action=\"https://apig.com/test/call\""
            reqResponse `shouldMatchBody` "validDigits=\"1*\""
            reqResponse `shouldMatchBody` "</GetDigits><Redirect>https://apig.com/test/thanks</Redirect>"
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
            query conn "select duration from callers where call_uuid = ? and ended_at is not null limit 1" [callUuid] :: IO [Only Int]
          duration `shouldBe` 23

callEndpointSetup :: Connection -> Int -> IO ()
callEndpointSetup conn callerId = do
  insertTestCaller conn callerId
  insertTestTarget conn

surveyEndpointSetup :: Connection -> Int -> IO ()
surveyEndpointSetup conn callerId = do
  insertTestCaller conn callerId
  insertTestTarget conn
  -- insertTestCall conn callerId

insertTestCall :: Connection -> Int -> IO [Only Int]
insertTestCall conn callerId = do
  let testCall = (callerId, 1 :: Int)
  query conn "insert into calls (caller_id, target_id, created_at) values (?, ?, now()) returning id" testCall :: IO [Only Int]

insertTestCaller :: Connection -> Int -> IO ()
insertTestCaller conn callerId = do
  let testCaller = (callerId, 1 :: Int, "61411111111" :: String, "xxxxx" :: String)
  _ <-
    execute
      conn
      "insert into callers (id, campaign_id, number, call_uuid, created_at) values (?, ?, ?, ?, now())"
      testCaller
  return ()

insertTestTarget :: Connection -> IO ()
insertTestTarget conn = do
  let testTarget = (1 :: Int, "61400000000" :: String, "Test Target" :: String)
  _ <- execute conn "insert into targets (campaign_id, number, name, active) values (?, ?, ?, true)" testTarget
  return ()

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

bShow :: Int -> BS.ByteString
bShow = BS.packChars . show

shouldMatchBody :: APIGatewayProxyResponse Text -> Text -> Expectation
shouldMatchBody (APIGatewayProxyResponse _ _ (Just (TextValue body))) fragment =
  unpack body `shouldContain` unpack fragment
shouldMatchBody (APIGatewayProxyResponse 404 _ _) _ = error "response was 404"
shouldMatchBody _ _ = error "Request was probably malformed"

shouldNotMatchBody :: APIGatewayProxyResponse Text -> Text -> Expectation
shouldNotMatchBody (APIGatewayProxyResponse _ _ (Just (TextValue body))) fragment =
  unpack body `shouldNotContain` unpack fragment
shouldNotMatchBody (APIGatewayProxyResponse 404 _ _) _ = error "response was 404"
shouldNotMatchBody _ _ = error "Request was probably malformed"
