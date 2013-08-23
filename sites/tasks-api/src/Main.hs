{-# LANGUAGE OverloadedStrings #-}
module Main where
import Web.Scotty

main = scotty 3000 $ do
  get "/ping" $ text "pong"
  -- config <- getConfig
  -- logSuccess (logger config) "tasks-api initialized"
  -- runServer config router
