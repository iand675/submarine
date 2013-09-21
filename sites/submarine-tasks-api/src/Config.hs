{-# LANGUAGE TemplateHaskell #-}
module Config where
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Pool
import Data.Text
import Data.Yaml
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.Redis as Redis
import qualified Network.AMQP as Rabbit
import System.Environment
import qualified Web.Scotty as S

import Submarine.Common.Models
import Submarine.Data.Config
import Submarine.Data.Redis
import Submarine.Data.PostgreSQL
import qualified Submarine.Data.Tasks as T
import qualified Submarine.Data.Users as U
import Submarine.Errors
import Submarine.JSON
import Submarine.Models.Accounts
import Submarine.Models.Task

data ConfigSection = ConfigSection
  { configSectionRedisSettings :: RedisConfig
  , configSectionRabbitSettings :: RabbitConfig
  , configSectionPostgresSettings :: PostgresConfig
  }

jsonize ''ConfigSection

data ConfigSections = ConfigSections
	{ configSectionsUserBackend :: ConfigSection
	, configSectionsTaskBackend :: ConfigSection
	}

jsonize ''ConfigSections

data Config = Config
	{ taskBackend :: T.TaskBackend IO
	, userBackend :: U.UserBackend IO
	}

type HandlerM = ReaderT Config S.ActionM
type Handler = HandlerM ()

getConfig :: IO Config
getConfig = do
  -- desiredEnv <- lookupEnv "SUBMARINE"
  -- chosenEnv <- case desiredEnv of
  --   Nothing -> do
  --     putStrLn "No environment specified in the SUBMARINE environment variable. Defaulting to development."
  --     return Development
  --   Just str -> if str == "production"
  --     then return Production
  --     else return Development
	mConfigSettings <- loadConfig
	case mConfigSettings of
		Nothing -> error "No configuration file"
		Just c -> do
			tasksRedisConn <- initializeRedisConnectionPool $ configSectionRedisSettings $ configSectionsTaskBackend c
			tasksPgConn <- initializePostgresConnectionPool $ configSectionPostgresSettings $ configSectionsTaskBackend c
			userRedisConn <- initializeRedisConnectionPool $ configSectionRedisSettings $ configSectionsUserBackend c
			userPgConn <- initializePostgresConnectionPool $ configSectionPostgresSettings $ configSectionsUserBackend c
			return $ Config
				{ taskBackend = T.initializeTaskBackend tasksRedisConn tasksPgConn
				, userBackend = U.initializeUserBackend userRedisConn userPgConn
				}

createTask :: NewTask -> HandlerM (Either SingleValueError (Entity FullTask))
createTask n = do
  b <- taskBackend <$> ask
  liftIO $ T.createTask b $ n

getTask :: Id Task -> HandlerM (Either SingleValueError (Maybe FullTask))
getTask i = do
  b <- taskBackend <$> ask
  liftIO $ T.getTask b $ i

listTasks :: TaskQuery -> HandlerM [Entity Task]
listTasks q = do
  b <- taskBackend <$> ask
  liftIO $ T.listTasks b $ q

updateTask :: Id Task -> TaskPatch -> HandlerM (Either SingleValueError (Maybe FullTask))
updateTask i t = do
  b <- taskBackend <$> ask
  liftIO $ (T.updateTask b) i t

createUser :: NewUser -> HandlerM (Either SingleValueError User)
createUser u = do
	b <- userBackend <$> ask
	liftIO $ (U.createUser b) u

getUser :: Text -> HandlerM (Either SingleValueError (Maybe User))
getUser u = do
	b <- userBackend <$> ask
	liftIO $ (U.getUser b) u

listUsers :: HandlerM [User]
listUsers = do
	b <- userBackend <$> ask
	liftIO $ U.listUsers b All
