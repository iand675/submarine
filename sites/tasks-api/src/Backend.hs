{-# LANGUAGE PolyKinds #-}
module Backend where
-- import AMQP
{-import Database.Redis.Simple-}
{-import Database.PostgreSQL.Simple-}

newtype Identity a = Identity { fromIdentity :: a }

-- stubs
data User
data List
data Category
data Team
data Organization

data Task f = Task
	{ taskName :: f String
	, taskComplete :: f Bool
	}

data NewTask = NewTask
	{ newTaskName :: String
	}

type FullTask = Task Identity
type TaskPatch = Task Maybe

newtype Id a = Id String

-- backends should have invariants established but keep clever stuff under the hood?
-- leaning towards yes...

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

data UserBackend m = UserBackend
  { createUser :: NewUser -> m (Id User, FullUser)
  , getUser    :: Id User -> m (Maybe FullUser)
  , updateUser :: Id User -> UserPatch -> m (Maybe FullUser)
  , deleteUser :: Id User -> m (Maybe FullUser)
  , listUsers  :: UserQuery -> m [(Id User, FullUser)]
  }

data ListBackend m = ListBackend
  { createList :: NewList -> m (Id List, FullList)
  , getList    :: Id List -> m (Maybe List)
  , updateList :: Id List -> ListPatch -> m (Maybe List)
  , deleteList :: Id List -> m (Maybe List)
  , listLists  :: ListQuery -> m [(Id List, FullList)]
  }

-- data CategoryBackend m
-- data TeamBackend m
-- data OrganizationBackend m

data TaskQuery
	= All
	| Where [QueryPredicate]

data QueryPredicate
	= AssignedTo (Id User)
	| CreatedBy  (Id User)
	| ListIs     (Id List)
	| ProjectIs  (Id Project)
	| HasTags    [String]

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

createTaskImpl nt = do
  newTask <- initializeDefaults nt
  newTaskId <- postgres $ do
    insert into Tasks newTask
  redis $ do
    setEx (taskNamespace newTaskId) newTask
  amqp $ publish tasksQueue 




