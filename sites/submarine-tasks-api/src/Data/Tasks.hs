{-# LANGUAGE OverloadedStrings #-}
module Data.Tasks where
import Control.Monad
import Control.Monad.Trans
import Data.Aeson (encode)
import Data.ByteString.Char8 (ByteString, pack)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import Database.PostgreSQL.Simple

import AMQP
import Database.Redis.Simple
import Data.Types
import Data.Utility

{-
	forall TaskBackend:
		set task fields on new task match corresponding fields on full task
		getting task with returned id returns correct full task
	for task backend with caching
	  createTask caches created full task
	for task backend with event publishing	
	  createTask emits task created event

	  updateTask emits task updated event
	  deleteTask emits task deleted event
-}

taskExchange :: String
taskExchange = "1_tasks"

taskNamespace :: Show a => a -> ByteString
taskNamespace x = "tasks:" <> pack (show x)

taskCreated = undefined
taskUpdated = undefined

