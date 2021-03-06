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
      previousCall <- checkForIncompleteSurvey conn campaignId fromNumber
      case previousCall of
        [Just (callId, targetName)] -> do
          let responseUrl = appUrl "/survey_response?call_id=" <> pack (show callId)
          pure $ xmlResponse $ plivoResponse $ do
            getDigits responseUrl $ do
              wait
              speak $ "Welcome back! How did the call to " <> targetName <> " go?"
              speak survey
            redirect $ appUrl "/thanks"
        _ -> do
          [campaign] <- selectCampaign conn campaignId
          let callUrl = appUrl "/call"
          case campaign of
            (_, _, Just audio_url) ->
              pure $ xmlResponse $ plivoResponse $ do
                callDigits callUrl $ do
                  play audio_url
                  wait
                  speak "To make your first call, press 1"
                redirect $ appUrl "/thanks"
            (campaignName, Just instructions, _) ->
              let sentences = Data.Text.splitOn ". " instructions
               in pure $ xmlResponse $ plivoResponse $ do
                    speak $ "Welcome to the " <> campaignName <> " campaign."
                    wait
                    callDigits callUrl $ do
                      mapM_ toXML $ Prelude.concatMap (\x -> [speak x, wait]) sentences
                      speak "To make your first call, press 1"
                    redirect $ appUrl "/thanks"
            _ -> pure $ xmlResponse $ plivoResponse $ speak "This campaign is not configured correctly. Good bye"
    ("/call", Params {callUuidParam = Just callUuid}) -> do
      [(callerId, campaignId, callerNumber)] <- selectCaller conn callUuid
      target <- selectTargetNotCalledByCaller conn callerNumber campaignId
      case target of
        [(targetId, targetName, targetNumber)] -> do
          [Only callId] <- insertCall conn (callerId, targetId)
          pure $ xmlResponse $ plivoResponse $ do
            speak $ "Calling " <> targetName
            dial (appUrl "/survey?call_id=" <> tShow callId) targetNumber (appUrl "/log")
        _ ->
          pure $ xmlResponse $ plivoResponse $ do
            speak "All the targets have been called. Great work!"
            redirect $ appUrl "/thanks"
    ("/survey", Params { callIdParam = Just callId
                       , dialBLegUUIDParam = Just dialBLegUUID
                       , dialHangupCauseParam = Just dialHangupCause
                       , dialStatusParam = Just dialStatus
                       }) -> do
      _ <- updateCall conn (dialBLegUUID, dialHangupCause, dialStatus, callId)
      case dialStatus of
        "completed" ->
          let responseUrl = appUrl "/survey_response?call_id=" <> wrap callId
           in pure $ xmlResponse $ plivoResponse $ do
                speak "The call has ended."
                getDigits responseUrl $ speak survey
                redirect $ appUrl "/thanks"
        _ ->
          pure $ xmlResponse $ plivoResponse $ do
            speak "The office did not pickup."
            redirect $ appUrl "/call"
    ("/survey_response", Params {callIdParam = Just callId, digitsParam = Just digits}) -> do
      _ <- recordOutcome conn (outcomeText digits, callId)
      let nextUrl = appUrl "/next"
      pure $ xmlResponse $ plivoResponse $ do
        speak "Outcome received. Thank you."
        callDigits nextUrl $ speak "To call another office, press 1. To end the calling session, press star."
        redirect $ appUrl "/thanks"
    ("/next", Params {digitsParam = Just "1"}) -> pure $ xmlResponse $ plivoResponse $ redirect $ appUrl "/call"
    ("/next", Params {digitsParam = Just "*"}) -> pure $ xmlResponse $ plivoResponse $ redirect $ appUrl "/thanks"
    ("/thanks", _) ->
      pure $ xmlResponse $ plivoResponse $
      speak
        "Thank you for calling. We will contact you about next steps on the campaign. Until then, goodbye and have a great day!"
    ("/disconnect", Params {callUuidParam = Just callUuid, durationParam = Just duration}) -> do
      _ <- execute conn updateCaller (duration, callUuid)
      pure xmlResponseOk
    ("/stats", _) -> do
      [Only calls] <- query_ conn "select count(*) from calls" :: IO [Only Int]
      pure $ xmlResponse $ plivoResponse $ speak $ "Calls: " <> pack (show calls)
    ("/log", _) -> do
      print request
      pure responseOK
    (_, _) -> pure response404

dbUrl :: IO BS.ByteString
dbUrl = do
  envUrl <- lookupEnv "DATABASE_URL"
  return $ BS.packChars $ fromMaybe "postgresql://localhost/multi_targeter" envUrl

survey :: Text
survey =
  "If you had a meaningful conversation, press 1. If you reached an answering machine, press 2. If you were hung up on, press 3."

outcomeText :: BS.ByteString -> BS.ByteString
outcomeText digit =
  case digit of
    "1" -> "conversation"
    "2" -> "answering machine"
    "3" -> "hung up on"
    _ -> "unknown"

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
    , digitsParam :: Maybe BS.ByteString
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
    , digitsParam = lookupBody request "Digits"
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

selectCampaign :: Connection -> BS.ByteString -> IO [(Text, Maybe Text, Maybe Text)]
selectCampaign conn campaignId =
  let sql = "select name, instructions, audio_instructions_url from campaigns where id = ?"
   in query conn sql [campaignId] :: IO [(Text, Maybe Text, Maybe Text)]

selectTargetNotCalledByCaller :: Connection -> Text -> Int -> IO [(Int, Text, Text)]
selectTargetNotCalledByCaller conn callerNumber campaignId =
  query
    conn
    "select t.id, t.name, t.number \
    \from targets t \
    \left join calls c1 on c1.target_id = t.id \
    \left join calls c2 on c2.target_id = t.id \
    \  and c2.caller_id in (select id from callers where number = ?) \
    \where campaign_id = ? and active and c2.id is null \
    \group by 1,2,3 \
    \order by count(c1.*), random() \
    \limit 1"
    (callerNumber, campaignId) :: IO [(Int, Text, Text)]

checkForIncompleteSurvey :: Connection -> BS.ByteString -> BS.ByteString -> IO [Maybe (Int, Text)]
checkForIncompleteSurvey conn campaignId callerNumber =
  query
    conn
    "select c.id, t.name \
    \from callers cr \
    \join calls c on c.caller_id = cr.id \
    \join targets t on c.target_id = t.id \
    \where c.outcome is null \
    \  and cr.number = ? \
    \  and cr.campaign_id = ? \
    \order by cr.id desc \
    \limit 1"
    (callerNumber, campaignId) :: IO [Maybe (Int, Text)]

selectCaller :: Connection -> BS.ByteString -> IO [(Int, Int, Text)]
selectCaller conn callUuid =
  query conn "select id, campaign_id, number from callers where call_uuid = ? limit 1" [callUuid]

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

recordOutcome :: Connection -> (BS.ByteString, BS.ByteString) -> IO ()
recordOutcome conn params = do
  _ <- execute conn "update calls set outcome = ? where id = ?" params
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

play :: Text -> XML
play = Text.XML.Writer.element "Play" . content

speak :: Text -> XML
speak = Text.XML.Writer.elementA "Speak" [("language", "en-GB"), ("voice", "MAN")]

wait :: XML
wait = Text.XML.Writer.elementA "Wait" [("length", "1")] Text.XML.Writer.empty

callDigits :: Text -> XML -> XML
callDigits url inner =
  let options = [("action", url), ("numDigits", "1"), ("retries", "3"), ("validDigits", "1*")]
   in Text.XML.Writer.elementA "GetDigits" options inner

getDigits :: Text -> XML -> XML
getDigits url inner =
  let options = [("action", url), ("retries", "3"), ("numDigits", "1")]
   in Text.XML.Writer.elementA "GetDigits" options inner

redirect :: Text -> XML
redirect = Text.XML.Writer.element "Redirect" . content

dial :: Text -> Text -> Text -> XML
dial url number callbackUrl =
  let inner = Text.XML.Writer.element "Number" $ content number
      options =
        [ ("action", url)
        , ("hangupOnStar", "true")
        , ("timeout", "30")
        , ("timeLimit", "1800")
        , ("callbackUrl", callbackUrl)
        ]
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
