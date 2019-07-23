{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           AWSLambda.Events.APIGateway
import           Control.Lens
import           Data.Aeson
import qualified Data.HashMap.Strict         as HashMap
import           Data.Text
import           Handler
import           Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Handler" $ do
      context "with an empty request" $ do
        it "returns hello world in xml" $
          handler fakeRequest `shouldReturn`
          okXmlWithBody
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Response><Speak>Hello from haskell</Speak></Response>"

okXmlWithBody :: String -> APIGatewayProxyResponse Text
okXmlWithBody text = xmlResponseOk & responseBody ?~ pack text

fakeRequest :: APIGatewayProxyRequest Text
fakeRequest =
  APIGatewayProxyRequest
    { _agprqResource = "/{proxy+}"
    , _agprqPath = "/test/hello"
    , _agprqHttpMethod = "GET"
    , _agprqHeaders =
        [ ("X-Forwarded-Proto", "https")
        , ("CloudFront-Is-Desktop-Viewer", "true")
        , ( "Accept"
          , "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
        , ( "X-Amz-Cf-Id"
          , "nBsWBOrSHMgnaROZJK1wGCZ9PcRcSpq_oSXZNQwQ10OTZL4cimZo3g==")
        , ("Accept-Encoding", "gzip, deflate, lzma, sdch, br")
        , ("CloudFront-Forwarded-Proto", "https")
        , ("Accept-Language", "en-US,en;q=0.8")
        , ("CloudFront-Is-Tablet-Viewer", "false")
        , ("Upgrade-Insecure-Requests", "1")
        , ("CloudFront-Viewer-Country", "US")
        , ( "User-Agent"
          , "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.82 Safari/537.36 OPR/39.0.2256.48")
        , ("CloudFront-Is-Mobile-Viewer", "false")
        , ("Host", "wt6mne2s9k.execute-api.us-west-2.amazonaws.com")
        , ("X-Forwarded-Port", "443")
        , ("CloudFront-Is-SmartTV-Viewer", "false")
        , ( "Via"
          , "1.1 fb7cca60f0ecd82ce07790c9c5eef16c.cloudfront.net (CloudFront)")
        , ("X-Forwarded-For", "192.168.100.1, 192.168.1.1")
        ]
    , _agprqQueryStringParameters = [("name", Just "me")]
    , _agprqPathParameters = HashMap.fromList [("proxy", "hello")]
    , _agprqStageVariables =
        HashMap.fromList [("stageVarName", "stageVarValue")]
    , _agprqRequestContext =
        ProxyRequestContext
          { _prcPath = Nothing
          , _prcAccountId = "123456789012"
          , _prcResourceId = "us4z18"
          , _prcStage = "test"
          , _prcRequestId = "41b45ea3-70b5-11e6-b7bd-69b5aaebc7d9"
          , _prcIdentity =
              RequestIdentity
                { _riCognitoIdentityPoolId = Just ""
                , _riAccountId = Just ""
                , _riCognitoIdentityId = Just ""
                , _riCaller = Just ""
                , _riApiKey = Just ""
                , _riSourceIp = Nothing
                , _riCognitoAuthenticationType = Just ""
                , _riCognitoAuthenticationProvider = Just ""
                , _riUserArn = Just ""
                , _riUserAgent =
                    Just
                      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.82 Safari/537.36 OPR/39.0.2256.48"
                , _riUser = Just ""
                }
          , _prcResourcePath = "/{proxy+}"
          , _prcHttpMethod = "GET"
          , _prcApiId = "wt6mne2s9k"
          , _prcProtocol = "HTTP/1.1"
          , _prcAuthorizer =
              Just
                Authorizer
                  { _aPrincipalId = Just "test-principalId"
                  , _aClaims =
                      HashMap.fromList
                        [ ("email", toJSON ("test@example.com" :: Text))
                        , ("email_verified", toJSON True)
                        ]
                  , _aContext =
                      HashMap.fromList [("custom_context", toJSON (10 :: Int))]
                  }
          }
    , _agprqBody = Nothing
    }
