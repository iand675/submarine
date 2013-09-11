{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Reader
import Web.Scotty
import Config
import HTTP.Tasks

main = do
  settings <- getConfig
  scotty 3000 $ do
	  get "/status" $ Web.Scotty.text "pong"
	  tasks settings
  -- config <- getConfig
  -- logSuccess (logger config) "tasks-api initialized"
  -- runServer config router

tasks settings = do
	let run m = runReaderT m settings
	-- options "/tasks"
	post "/tasks" $ run createTask
	get "/tasks" $ run listTasks
	get "/tasks/:taskId" $ run getTask
	patch "/tasks/:taskId" $ run updateTask