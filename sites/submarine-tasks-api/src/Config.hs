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

import Submarine.Data.Config
import Submarine.Data.Redis
import Submarine.Data.PostgreSQL
import qualified Submarine.Data.Tasks as T
import Submarine.Common.Models
import Submarine.JSON
import Submarine.Models.Task

data ConfigSection = ConfigSection
  { configSectionRedisSettings :: RedisConfig
  , configSectionRabbitSettings :: RabbitConfig
  , configSectionPostgresSettings :: PostgresConfig
  }

jsonize ''ConfigSection

data ConfigSettings = ConfigSettings
  { configSettingsProduction :: ConfigSection
  , configSettingsDevelopment :: ConfigSection
  }

jsonize ''ConfigSettings

data Config = Config
  { redisConnectionPool    :: Redis.Connection
  , postgresConnectionPool :: Pool Postgres.Connection
  , rabbitConnectionPool   :: Pool Rabbit.Connection
  , taskBackend            :: T.TaskBackend BackendM
  }

data Environment = Development | Production

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
			redisConn <- initializeRedisConnectionPool $ configSectionRedisSettings $ configSettingsDevelopment c
			return $ Config
				{ redisConnectionPool = redisConn
				, postgresConnectionPool = undefined
				, rabbitConnectionPool = undefined
				}

type BackendM = ReaderT Config IO

createTask :: NewTask -> BackendM (Id Task, FullTask)
createTask n = do
  b <- taskBackend <$> ask
  T.createTask b $ n

getTask :: Id Task -> BackendM (Maybe FullTask)
getTask i = do
  b <- taskBackend <$> ask
  T.getTask b $ i

listTasks :: TaskQuery -> BackendM [Entity Task]
listTasks q = do
  b <- taskBackend <$> ask
  T.listTasks b $ q

updateTask :: Id Task -> TaskPatch -> BackendM (Maybe FullTask)
updateTask i t = do
  b <- taskBackend <$> ask
  (T.updateTask b) i t

backend :: MonadIO m => BackendM a -> ReaderT Config m a
backend m = do
  c <- ask
  liftIO $ runReaderT m c
