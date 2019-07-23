{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           AWSLambda.Events.APIGateway
import           Control.Lens
import           Data.Text
import           Handler
import           Test.Hspec

import           Mocks

okXmlWithBody :: String -> APIGatewayProxyResponse Text
okXmlWithBody text = xmlResponseOk & responseBody ?~ pack text

main :: IO ()
main =
  hspec $ do
    describe "Handler" $ do
      context "with an empty request" $ do
        it "returns hello world in xml" $
          handler Mocks.request `shouldReturn`
          okXmlWithBody
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Response><Speak>Hello from haskell</Speak></Response>"
