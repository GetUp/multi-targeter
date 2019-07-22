{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           AWSLambda
import           Prelude.Compat

import           Data.Aeson                 (Value, decode, encode)
import           Data.Aeson.TH              (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as BL

data Response =
  Response
    { statusCode :: Int
    , body       :: String
    }
  deriving (Show)

$(deriveJSON defaultOptions ''Response)

main = lambdaMain handler

handler :: Value -> IO String
handler evt = do
  let reply = Response {statusCode = 200, body = "<_-.-_>I'm working!</_-.-_>"}
  pure $ BL.unpack (encode reply)
