{-# LANGUAGE OverloadedStrings #-}

module Handler where

import           AWSLambda.Events.APIGateway
import           Control.Lens
import qualified Data.ByteString.Internal    as BS
import           Data.Text
import qualified Data.Text.Lazy              as LazyText
import           Text.XML
import           Text.XML.Writer

handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler request = do
  let urlPath = BS.unpackChars $ request ^. agprqPath
  case urlPath of
    "/connect" ->
      pure $
      xmlResponse $
      plivoResponse $ do
        speak "Welcome to the Test Campaign."
        speak
          "We will attempt to connect to you to one of their 40 offices. If someone picks up and you have converstation, afterwards we will ask you questions about how it went. You then have the option to try another office. Press the star key to hang up at any point during the conversation. Remember to be polite."
        redirect $ appUrl request "/call"
    "/call" ->
      pure $
      xmlResponse $
      plivoResponse $ do
        speak "Calling the Sydney office."
        dial (appUrl request "/survey") "61285994347"
    "/survey" ->
      pure $
      xmlResponse $
      plivoResponse $ do
        speak "The call has ended."
        redirect $ appUrl request "/call"
    "/disconnect" -> pure $ xmlResponseOk
    _ -> pure $ xmlResponse "I'm awake!"

xmlResponseOk :: APIGatewayProxyResponse body
xmlResponseOk =
  APIGatewayProxyResponse 200 [("Content-Type", "text/xml")] Nothing

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
  Text.XML.Writer.elementA "Dial" [("action", url), ("hangupOnStart", "true")] $ do
    Text.XML.Writer.element "Number" $ content number

appUrl :: APIGatewayProxyRequest Text -> Text -> Text
appUrl request path = do
  let headers = request ^. agprqHeaders
  case lookup "Host" headers of
    Just host -> "https://" <> pack (BS.unpackChars host) <> path
    Nothing   -> error "Hostname not found"
