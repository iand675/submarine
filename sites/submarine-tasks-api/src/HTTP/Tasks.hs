{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module HTTP.Tasks where
import Control.Applicative
import Control.Monad.Reader
import Database.Redis.Simple hiding (Message)
import Data.Either
import Data.Monoid
import Data.Pool
import Data.Text (Text)
import qualified Data.Text.Lazy as L
import Data.UUID
import Network.HTTP.Types
import Submarine.Web.Actions
import Submarine.Web.Message
import qualified Web.Scotty as S

import Config
import Util
--import qualified Submarine.Tasks.Data as T
--import Submarine.
import Submarine.Models.Task
--import qualified Data.Tasks as Data

validateAndParseParam ks (k, v) = case lookup k ks of
	Nothing -> Left ("Invalid query parameter: " <> k)
	Just f -> Right $ f v

queryPredicateHandlers :: [(L.Text, L.Text -> Either L.Text QueryPredicate)]
queryPredicateHandlers = 
	[ ("assigned_to", fmap AssignedTo . S.parseParam)
	, ("created_by", fmap CreatedBy . S.parseParam)
	, ("list_is", fmap ListIs . S.parseParam )
	, ("category_is", fmap CategoryIs . S.parseParam)
	, ("has_tags", fmap HasTags . S.parseParamList)
	]

createTaskHandler :: Handler
createTaskHandler = do
	jsonData >>= createTask >>= whenOk json

listTasksHandler :: Handler
listTasksHandler = do
  -- need this to deal with key too, not just value
	ps <- params
	let (ls, rs) = partitionEithers $ map (validateAndParseParam queryPredicateHandlers) ps
	case ls of
		[] -> do
			-- ts <- backend $ listTasks $ Where rs
			json $ Tasks [] -- ts
		_ -> status badRequest400

getTaskHandler :: Handler
getTaskHandler = do
	param "taskId" >>= getTask >>= returnSingle

updateTaskHandler :: Handler
updateTaskHandler = do
	taskId <- param "taskId"
	taskPatch <- jsonData
	updateTask taskId taskPatch >>= returnSingle
--data Body
--data Segment segmentName a
--data Query parameterName a

--class RequestInfo a where
--  retrieveInfo :: Request -> Either (Handler ErrorMessage) a

--instance (SingE (Kind Symbol) String) => RequestInfo (Segment Symbol a) where
--  retrieveInfo = request .~ path .~ lookup (fromSing _)

--createTask :: Body NewTask -> Handler Task
--createTask = withAuth $ \user -> do
--  taskDto <- require "Invalid task" =<< body
--  newTask <- Data.createTask user $ domainModel taskDto
--  return $ webModel newTask .~ statusCode Created

--getTask :: Segment "taskId" TaskId -> Handler Task
--getTask taskId = withAuth $ \user -> do
--  task <- Data.getTask user taskId
--  return $ webModel task

--listTasks :: Query "createdBy" UserId -> Handler [Task]
--listTasks = withAuth $ \user -> do
--  tasks <- Data.listTasks user
--  return $ webModel tasks

--deleteTask :: Sgement "taskId" TaskId -> Handler Task
--deleteTask taskId = withAuth $ \user -> do
--  deletedTask <- Data.delete user taskId
--  return $ webModel

