{-# LANGUAGE OverloadedStrings #-}

module Handler where

import AWSLambda.Events.APIGateway
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Internal as BS
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as LazyText
import Database.PostgreSQL.Simple
import Network.HTTP.Types.URI (parseSimpleQuery)

import System.Environment
import Text.XML
import Text.XML.Writer

handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler request = do
  let appUrl = buildAppUrl request
  let urlPath = BS.unpackChars $ request ^. agprqPath
  url <- dbUrl
  conn <- connectPostgreSQL url
  case (urlPath, buildParams request) of
    ("/connect", Params { campaignIdParam = Just campaignId
                        , fromNumberParam = Just fromNumber
                        , callUuidParam = Just callUuid
                        }) -> do
      _ <- execute conn insertCaller (fromNumber, wrap campaignId, callUuid)
      pure $ xmlResponse $ plivoResponse $ do
        speak "Welcome to the Test Campaign."
        speak
          "We will attempt to connect to you to one of their 40 offices. If someone picks up and you have conversation, afterwards we will ask you questions about how it went. You then have the option to try another office. Press the star key to hang up at any point during the conversation. Remember to be polite."
        redirect $ appUrl "/call"
    ("/call", Params {callUuidParam = Just callUuid}) -> do
      [Only callerId] <- selectCaller conn callUuid
      [(targetId, targetName, targetNumber)] <- selectTarget conn
      [Only callId] <- insertCall conn (callerId, targetId)
      pure $ xmlResponse $ plivoResponse $ do
        speak $ "Calling the " <> targetName
        dial (appUrl "/survey?call_id=" <> tShow callId) targetNumber
    ("/survey", Params { callIdParam = Just callId
                       , dialBLegUUIDParam = Just dialBLegUUID
                       , dialHangupCauseParam = Just dialHangupCause
                       , dialStatusParam = Just dialStatus
                       }) -> do
      _ <- updateCall conn (dialBLegUUID, dialHangupCause, dialStatus, callId)
      pure $ xmlResponse $ plivoResponse $ do
        speak "The call has ended."
        redirect $ appUrl "/call"
    ("/disconnect", Params {callUuidParam = Just callUuid, durationParam = Just duration}) -> do
      _ <- execute conn updateCaller (duration, callUuid)
      pure xmlResponseOk
    (_, _) -> pure response404

dbUrl :: IO BS.ByteString
dbUrl = do
  envUrl <- lookupEnv "DATABASE_URL"
  return $ BS.packChars $ fromMaybe "postgresql://localhost/multi_targeter" envUrl

data Params =
  Params
    { campaignIdParam :: Maybe BS.ByteString
    , fromNumberParam :: Maybe BS.ByteString
    , callUuidParam :: Maybe BS.ByteString
    , durationParam :: Maybe BS.ByteString
    , callIdParam :: Maybe BS.ByteString
    , dialBLegUUIDParam :: Maybe BS.ByteString
    , dialHangupCauseParam :: Maybe BS.ByteString
    , dialStatusParam :: Maybe BS.ByteString
    }

buildParams :: APIGatewayProxyRequest Text -> Params
buildParams request =
  Params
    { campaignIdParam = lookupParam request "campaign_id"
    , fromNumberParam = lookupBody request "From"
    , callUuidParam = lookupBody request "CallUUID"
    , durationParam = lookupBody request "Duration"
    , callIdParam = lookupParam request "call_id"
    , dialBLegUUIDParam = lookupBody request "DialBLegUUID"
    , dialHangupCauseParam = lookupBody request "DialHangupCause"
    , dialStatusParam = lookupBody request "DialStatus"
    }

lookupParam :: APIGatewayProxyRequest Text -> BS.ByteString -> Maybe BS.ByteString
lookupParam request param = do
  let params = request ^. agprqQueryStringParameters
  join $ lookup param params

lookupBody :: APIGatewayProxyRequest Text -> BS.ByteString -> Maybe BS.ByteString
lookupBody request param = do
  body <- request ^. requestBody
  let params = parseSimpleQuery $ encodeUtf8 body
  lookup param params

selectTarget :: Connection -> IO [(Int, Text, Text)]
selectTarget conn =
  query_ conn "select id, name, number from targets order by random() limit 1" :: IO [(Int, Text, Text)]

selectCaller :: Connection -> BS.ByteString -> IO [Only Int]
selectCaller conn callUuid = query conn "select id from callers where call_uuid = ? limit 1" [callUuid] :: IO [Only Int]

insertCall :: Connection -> (Int, Int) -> IO [Only Int]
insertCall conn call =
  query conn "insert into calls (caller_id, target_id, created_at) values (?, ?, now()) returning id" call :: IO [Only Int]

updateCall :: Connection -> (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString) -> IO ()
updateCall conn callParams = do
  _ <-
    execute
      conn
      "update calls set call_uuid = ?, hangup_cause = ?, status = ?, ended_at = now() where id = ?"
      callParams
  return ()

insertCaller :: Query
insertCaller = "insert into callers (number, campaign_id, call_uuid, created_at) values (?, ?, ?, now())"

updateCaller :: Query
updateCaller = "update callers set ended_at = now(), duration = ? where call_uuid = ?"

xmlResponseOk :: APIGatewayProxyResponse body
xmlResponseOk = APIGatewayProxyResponse 200 [("Content-Type", "text/xml")] Nothing

response404 :: APIGatewayProxyResponse Text
response404 = APIGatewayProxyResponse 404 [] Nothing

xmlResponse :: Text -> APIGatewayProxyResponse Text
xmlResponse text = xmlResponseOk & responseBody ?~ text

plivoResponse :: XML -> Text
plivoResponse = LazyText.toStrict . renderText def . document "Response"

speak :: Text -> XML
speak = Text.XML.Writer.element "Speak" . content

redirect :: Text -> XML
redirect = Text.XML.Writer.element "Redirect" . content

dial :: Text -> Text -> XML
dial url number =
  let inner = Text.XML.Writer.element "Number" $ content number
      options = [("action", url), ("hangupOnStar", "true")]
   in Text.XML.Writer.elementA "Dial" options inner

tShow :: Int -> Text
tShow = pack . show

wrap :: BS.ByteString -> Text
wrap s = pack $ BS.unpackChars s

buildAppUrl :: APIGatewayProxyRequest Text -> Text -> Text
buildAppUrl request path = do
  let headers = request ^. agprqHeaders
  case lookup "Host" headers of
    Nothing -> error "Hostname not found"
    Just host -> do
      let stage = request ^. agprqRequestContext . prcStage
      "https://" <> wrap host <> "/" <> stage <> path
