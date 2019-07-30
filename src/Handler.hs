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
  let params = buildParams request
  case (urlPath, params) of
    ("/connect", Params { campaignIdParam = Just campaignId
                        , fromNumberParam = Just fromNumber
                        , callUuidParam = Just callUuid
                        }) -> do
      url <- dbUrl
      conn <- connectPostgreSQL url
      _ <-
        execute
          conn
          "insert into callers (number, campaign_id, call_uuid, created_at) values (?, ?, ?, now())"
          (fromNumber, wrap campaignId, callUuid)
      pure $ xmlResponse $ plivoResponse $ do
        speak "Welcome to the Test Campaign."
        speak
          "We will attempt to connect to you to one of their 40 offices. If someone picks up and you have conversation, afterwards we will ask you questions about how it went. You then have the option to try another office. Press the star key to hang up at any point during the conversation. Remember to be polite."
        redirect $ appUrl "/call"
    ("/call", _) -> do
      url <- dbUrl
      conn <- connectPostgreSQL url
      [(targetName, targetNumber)] <- query_ conn randomTarget :: IO [(Text, Text)]
      pure $ xmlResponse $ plivoResponse $ do
        speak $ "Calling the " <> targetName
        dial (appUrl "/survey") targetNumber
    ("/survey", _) ->
      pure $ xmlResponse $ plivoResponse $ do
        speak "The call has ended."
        redirect $ appUrl "/call"
    ("/disconnect", Params {callUuidParam = Just callUuid, durationParam = Just duration}) -> do
      url <- dbUrl
      conn <- connectPostgreSQL url
      _ <- execute conn "update callers set ended_at = now(), duration = ? where call_uuid = ?" (duration, callUuid)
      pure xmlResponseOk
    (_, _) -> pure $ response404

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
    }

buildParams :: APIGatewayProxyRequest Text -> Params
buildParams request =
  Params
    { campaignIdParam = lookupParam request "campaign_id"
    , fromNumberParam = lookupBody request "From"
    , callUuidParam = lookupBody request "CallUUID"
    , durationParam = lookupBody request "Duration"
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

randomTarget :: Query
randomTarget = "select name, number from targets order by random() limit 1"

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
