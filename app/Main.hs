module Main where

import AWSLambda.Events.APIGateway
import Handler

main :: IO ()
main = apiGatewayMain handler
