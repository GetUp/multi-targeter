{-# LANGUAGE OverloadedStrings #-}

module Handler where

import           AWSLambda.Events.APIGateway
import           Control.Lens
import           Data.ByteString.Internal
import           Data.Text
import qualified Data.Text.Lazy              as LazyText
import           Text.XML
import           Text.XML.Writer

handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler request = do
  let urlPath = unpackChars $ request ^. agprqPath
  case urlPath of
    "/test/hello" ->
      pure $ xmlResponse $ plivoResponse $ do speak "Hello from haskell"
    "/" -> pure $ xmlResponse $ plivoResponse $ do speak "Root"
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
