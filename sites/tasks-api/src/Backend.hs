{-# LANGUAGE PolyKinds #-}
module Backend where
-- import AMQP
{-import Database.Redis.Simple-}
{-import Database.PostgreSQL.Simple-}

newtype Identity a = Identity { fromIdentity :: a }

-- stubs
data User
data List
data Project

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

data TaskBackend m = TaskBackend
	{ createTask :: NewTask -> m (Id Task, FullTask)
	, readTask   :: Id Task -> m (Maybe FullTask)
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
