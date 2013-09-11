{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module HTTP.Tasks where
import Control.Applicative
import Control.Monad.Reader
import Database.Redis.Simple
import Data.Either
import Data.Monoid
import Data.Pool
import Data.UUID
import Network.HTTP.Types
import Webby.Actions
import qualified Web.Scotty as S

import Config
import qualified Data.Tasks as T
import Data.Utility
import Submarine.Models.Task
--import qualified Data.Tasks as Data

instance RedisBacked HandlerM where
  redis m = do
    conf <- ask
    liftIO $ withResource (redisConnectionPool conf) (\c -> runRedis c m)

emptyTask = Task nil "" False

createTaskHandler :: Handler
createTaskHandler = do
  newTask <- jsonData
  createdTask <- backend $ createTask newTask
  status created201
  json createdTask

listTasksHandler :: Handler
listTasksHandler = do
  -- need this to deal with key too, not just value
  (ls, rs) <- (partitionEithers . map S.parseParam) <$> params
  case ls of
    [] -> do
      ts <- backend $ listTasks $ T.Where rs
      json $ Tasks ts
    _ -> status badRequest400

getTaskHandler :: Handler
getTaskHandler = do
  taskId <- param "taskId"
  task <- backend $ getTask taskId
  json task

updateTaskHandler :: Handler
updateTaskHandler = do
  taskId <- param "taskId"
  taskPatch <- jsonData
  updatedTask <- backend $ updateTask taskId taskPatch
  json updatedTask

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

