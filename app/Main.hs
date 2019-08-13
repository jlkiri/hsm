{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.Wai.Middleware.Static

showLandingPage :: ActionM ()
showLandingPage = do
  setHeader "Content-Type" "text/html"

routes :: ScottyM ()
routes = do
  middleware $ staticPolicy $ addBase "/static"

main :: IO ()
main = do
  putStrLn "Starting Server..."
  scotty 3000 routes
