{-# LANGUAGE OverloadedStrings #-}

module Handler where

import           AWSLambda.Events.APIGateway
import           Control.Lens
import qualified Data.ByteString.Lazy.Internal as BSL
import qualified Data.HashMap.Strict           as HMS
import           Data.Text
import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.Encoding       as LazyText
import Data.Aeson.TextValue
import Text.XML
import Text.XML.Writer

handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler _ = do
  pure $ xmlResponse $ document "Response" $ do
    Text.XML.Writer.element "Speak" $ content "Hello from haskell"

xmlResponseOk :: APIGatewayProxyResponse body
xmlResponseOk =
  APIGatewayProxyResponse 200 [("Content-Type", "text/xml")] Nothing

xmlResponse :: Document -> APIGatewayProxyResponse Text
xmlResponse doc = xmlResponseOk & responseBody ?~  LazyText.toStrict (renderText def doc)
