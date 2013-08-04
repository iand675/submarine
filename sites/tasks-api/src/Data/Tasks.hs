module Data.Tasks where
import Data.Types
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
data TaskBackend m = TaskBackend
	{ createTask :: NewTask -> m (Id Task, FullTask)
	, getTask    :: Id Task -> m (Maybe FullTask)
	, updateTask :: Id Task -> TaskPatch -> m (Maybe FullTask)
	, deleteTask :: Id Task -> m (Maybe FullTask)
	, listTasks  :: TaskQuery -> m [(Id Task, FullTask)]
	}

data TaskQuery
	= All
	| Where [QueryPredicate]

data QueryPredicate
	= AssignedTo (Id User)
	| CreatedBy  (Id User)
	| ListIs     (Id List)
	| ProjectIs  (Id Project)
	| HasTags    [String]

publishTaskEvents :: TaskBackend m -> TaskBackend m'
publishTaskEvents b = b
  { createTask = \t -> (createTask b) t >>= \p@(taskId, task) -> do
      lift $ publish taskExchange (taskCreated taskId task)
      return p
    updateTask = \t p -> (updateTask b) t p >>= \mUpdatedTask -> do
      case mUpdatedTask of
        Nothing -> return ()
        Just updatedTask -> lift $ publish taskExchange (taskUpdated taskId updatedTask)
      return mUpdatedTask
    deleteTask = \t -> (deleteTask b) t >>= \mDeletedTask -> do
      case mDeletedTask of
        Nothing -> return ()
        Just deletedTask -> lift $ publish taskExchange (taskDeleted taskId deletedTask)
      return mDeletedTask
  }

cacheTasks :: TaskBackend m -> TaskBackend m'
cacheTasks b = b
  { createTask = \t -> (createTask b) t >>= \p@(taskId, task) -> do
      setCached taskId task
      return p
  , getTask = \taskId -> do
      mtask <- getCached
      case mtask of
        Nothing -> do
          mtask' <- (getTask b) taskId
          maybe (return ()) (setCached taskId) t
          return mtask'
        foundTask -> return foundTask
  , updateTask = \t p -> do
      updatedTask <- (updateTask b) t p
      maybe (return ()) (setCached t) updatedTask
      return updatedTask
  , deleteTask = \t -> do
      mdeletedTask <- (deleteTask b) t
      maybe (return ()) (\_ -> removeCached t) mdeletedTask
      return mdeletedTask
  }
  where
    getCached taskId = do
      mTask <- lift $ get (taskNamespace taskId)
      lift $ expire (taskNamespace taskId) 86400
      return $ maybe Nothing decode mTask
    setCached taskId task = lift $ setEx (taskNamespace taskId) 86400 (encode task)
    removeCached taskId = lift $ del [taskNamespace taskId]

inMemoryBackend :: IO (TaskBackend IO)
inMemoryBackend = do
  undefined -- some simple implementation
	return $ TaskBackend
		{ createTask = undefined
		, readTask   = undefined
		, updateTask = undefined
		, deleteTask = undefined
		, listTasks  = undefined
		}

createTaskImp :: NewTask -> m (Id Task, FullTask)
createTaskImpl nt = do
  newTask <- initializeDefaults nt
  newTaskId <- postgres $ do
    insert into Tasks newTask



