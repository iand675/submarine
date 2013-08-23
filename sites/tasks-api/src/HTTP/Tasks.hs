module HTTP.Tasks where

data Body
data Segment segmentName a
data Query parameterName a

class RequestInfo a where
  retrieveInfo :: Request -> Either (Handler ErrorMessage) a

instance (SingE (Kind Symbol) String) => RequestInfo (Segment Symbol a) where
  retrieveInfo = request .~ path .~ lookup (fromSing _)

createTask :: Body NewTask -> Handler Task
createTask = withAuth $ \user -> do
  taskDto <- require "Invalid task" =<< body
  newTask <- Data.createTask user $ domainModel taskDto
  return $ webModel newTask .~ statusCode Created

getTask :: Segment "taskId" TaskId -> Handler Task
getTask taskId = withAuth $ \user -> do
  task <- Data.getTask user taskId
  return $ webModel task

listTasks :: Query "createdBy" UserId -> Handler [Task]
listTasks = withAuth $ \user -> do
  tasks <- Data.listTasks user
  return $ webModel tasks

deleteTask :: Sgement "taskId" TaskId -> Handler Task
deleteTask taskId = withAuth $ \user -> do
  deletedTask <- Data.delete user taskId 
  return $ webModel
