{-# LANGUAGE QuasiQuotes, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, TypeOperators #-}
module Submarine.Data.Tasks where
import Control.Applicative
import Data.Aeson (encode, decode)
import Data.ByteString.Char8 (ByteString, pack)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Monoid ((<>))
import Data.Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple hiding (Connection)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import qualified Database.PostgreSQL.Simple as Postgres
import Database.PostgreSQL.Simple.SqlQQ
import Database.Redis as Redis (Connection)
import Database.Redis.Simple hiding (decode)
import qualified Network.AMQP as Rabbit (Connection)
import Submarine.AMQP.Schema
import Submarine.Common.Models
import Submarine.Data.PostgreSQL
import Submarine.Data.Util
import Submarine.Errors
import Submarine.Models.Task

taskNamespace :: Id Task -> ByteString
taskNamespace x = "task:" <> pack (show x)

data DbTask = DbTask { dbTaskName :: Text, dbTaskComplete :: Bool, dbTaskCompletedBy :: Int }

dbTask :: Int -> Task -> DbTask
dbTask u (Task tn c) = DbTask tn c u

instance FromRow DbTask where
	fromRow = DbTask <$> field <*> field <*> field

instance ToRow DbTask where
	toRow (DbTask tn c cb) = [toField tn, toField c, toField cb]

fromDbTask :: DbTask -> Task
fromDbTask (DbTask tn c cb) = Task tn c

toEntity :: (a -> b) -> (Only (Id b) :. a) -> Entity b
toEntity f ((Only i) :. x) = Entity i (f x)

data TaskBackend m = TaskBackend
	{ createTask :: NewTask -> m (Either SingleValueError (Entity Task))
	, getTask    :: Id Task -> m (Either SingleValueError (Maybe Task))
	, updateTask :: Id Task -> Task -> m (Either SingleValueError (Maybe Task))
	, listTasks  :: TaskQuery -> m [Entity Task]
	}

initializeTaskBackend :: Redis.Connection -> Pool Postgres.Connection -> TaskBackend IO
initializeTaskBackend redisConn pgConn = cacheTasks redisConn $ postgresBackend pgConn

cacheTasks :: Redis.Connection -> TaskBackend m -> TaskBackend m
cacheTasks c b = b
  { createTask = cacheNewTask . (createTask b)
  , getTask = cacheRetrievedTask . (getTask b)
  , updateTask = cacheUpdatedTask . (updateTask b)
  }
  where
    getCached taskId = do
      mTask <- get (taskNamespace taskId)
      expire (taskNamespace taskId) 86400
      return $ maybe Nothing (decode . fromStrict) mTask
    setCached taskId task = do
    	setEx (taskNamespace taskId) 86400 (toStrict $ encode task)
    	return ()

cacheNewTask = id
cacheRetrievedTask = id
cacheUpdatedTask = id

type Single = Either SingleValueError
type PGPool = Pool Postgres.Connection
type PGAction a b = PGPool -> a -> IO b

postgresBackend :: PGPool -> TaskBackend IO
postgresBackend p = TaskBackend
  { createTask = createTaskImpl p
	, getTask    = getTaskImpl p
	, updateTask = updateTaskImpl p
	, listTasks  = listTasksImpl p
	}

initializeDefaults :: NewTask -> Task
initializeDefaults n = Task (newTaskName n) False

createTaskImpl :: PGAction NewTask (Single (Entity FullTask))
createTaskImpl p nt = withResource p $ \c -> do
	let newTask = dbTask 0 $ initializeDefaults nt
	rs <- query c "insert into tasks (name, complete, created_by) values (?, ?, ?)" newTask
	return $! exactlyOne $ map (toEntity fromDbTask) rs

getTaskImpl :: PGAction (Id Task) (Single (Maybe FullTask))
getTaskImpl p taskId = withResource p $ \c -> do
	rs <- query c [sql|
		select id, name, complete, created_by from tasks where id = ? limit 1
	|] (Only taskId)
	return $! oneOrNothing $ map fromDbTask rs

updateTaskImpl :: PGPool -> Id Task -> TaskPatch -> IO (Single (Maybe FullTask))
updateTaskImpl p taskId patch = withResource p $ \c -> do
	rs <- query c [sql|
		update Tasks set {Task} = _ where id = taskId;
		insert into TaskUpdates {Task} values ?;
		select {Task} from tasks where id = taskId limit 1;
	|] (Only taskId :. dbTask 0 patch)
	return $! oneOrNothing $ map fromDbTask rs

listTasksImpl :: PGAction TaskQuery [Entity FullTask]
listTasksImpl p q = withResource p $ \c -> do
	rs <- query_ c [sql| select {Task} from tasks where {{criterion}} |]
	return $! map (toEntity fromDbTask) rs
	{-
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
