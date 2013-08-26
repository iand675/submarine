module Submarine.Data.Tasks where
import Database.Redis.Simple
import Submarine.AMQP.Schema
import Submarine.Models.Common
import Submarine.Models.Tasks

data TaskBackend m = TaskBackend
	{ createTask :: NewTask -> m (Id Task, FullTask)
	, getTask    :: Id Task -> m (Maybe FullTask)
	, updateTask :: Id Task -> TaskPatch -> m (Maybe FullTask)
	, listTasks  :: TaskQuery -> m [Entity Task FullTask]
	}


-- taskBackend :: (RabbitBacked m, RedisBacked m, PostgresBacked m) => TaskBackend m
-- taskBackend = publishTaskEvents $ cacheTasks $ postgresBackend

--publishTaskEvents :: AMQPBacked m => TaskBackend m -> TaskBackend m
--publishTaskEvents b = b
--  { createTask = \t -> (createTask b) t >>= \p@(taskId, task) -> do
--      amqp $ publishMessage taskExchange (taskCreated taskId task)
--      return p
--  , updateTask = \t p -> (updateTask b) t p >>= \mUpdatedTask -> do
--      case mUpdatedTask of
--        Nothing -> return ()
--        Just updatedTask -> amqp $ publishMessage taskExchange (taskUpdated t updatedTask)
--      return mUpdatedTask
--  }

--cacheTasks :: (Monad m, RedisBacked m) => TaskBackend m -> TaskBackend m
--cacheTasks b = b
--  { createTask = \t -> (createTask b) t >>= \p@(taskId, task) -> do
--      redis $ setCached taskId task
--      return p
--  , getTask = \taskId -> do
--      mtask <- getCached
--      case mtask of
--        Nothing -> do
--          mtask' <- (getTask b) taskId
--          maybe (return ()) (setCached taskId) mtask'
--          return mtask'
--        foundTask -> return foundTask
--  , updateTask = \t p -> do
--      updatedTask <- (updateTask b) t p
--      maybe (return ()) (setCached t) updatedTask
--      return updatedTask
--  }
--  where
--    getCached taskId = redis $ do
--      mTask <- get (taskNamespace taskId)
--      expire (taskNamespace taskId) 86400
--      return $ maybe Nothing decode mTask
--    setCached :: (Monad m, RedisBacked m) => Id Task -> Task Identity -> m ()
--    setCached taskId task = do
--    	redis $ setEx (taskNamespace taskId) 86400 (toStrict $ encode task)
--    	return ()

{-
inMemoryBackend :: TaskBackend (State (Int, HashMap String FullTask))
inMemoryBackend = do
  undefined -- some simple implementation
	return $ TaskBackend
		{ createTask = undefined
		, getTask    = undefined
		, updateTask = undefined
		, listTasks  = undefined
		}
-}

{-
postgresBackend :: PostgresBacked m => TaskBackend m
postgresBackend = TaskBackend
  { createTask = createTaskImpl,
	, getTask = getTaskImpl,
	, updateTask = updateTaskImpl,
	, listTasks = listTasksImpl
	}

createTaskImpl :: NewTask -> m (Id Task, FullTask)
createTaskImpl nt = do
  newTask <- initializeDefaults nt
  newTaskId <- postgres $ do
    insert into Tasks newTask

getTaskImpl :: Id Task -> m (Maybe FullTask)
getTaskImpl taskId = single <$> [pgsql| select {Task} from tasks where id = @taskId limit 1 |]

updateTaskImpl :: Id Task -> TaskPatch -> m (Maybe FullTask)
updateTaskImpl taskId patch = single <$> [pgsql|
		update tasks set {Task} = _ where id = taskId;
		insert into TaskUpdates {Task} values @taskPatch;
		select {Task} from tasks where id = taskId limit 1;
	|]

listTasksImpl :: TaskQuery -> m [(Id Task, FullTask)]
listTasksImpl q = [pgsql| select {Task} from tasks where {{criterion}} |]
  where
		criterion = formatQueryPredicate q
		formatQueryPredicate (AssignedTo (Id userId)) = case p of
			(AssignedTo (Id userId)) -> "assigned_to = {userId}"
			(CreatedBy (Id userId)) -> "created_by = {userId}"
			(ListIs (Id listId)) -> "list_id = {listId}"
			-- TODO: this should induce a join from task -> listId -> list -> projectId -> project
			(ProjectIs (Id projectId)) -> "project_id = {listId}"
			-- TODO: this should induce a join on a tags table (postgres arrays would be inappropriate)
			(HasTags ts) -> "tags"
-}