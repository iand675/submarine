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

import Data.Tasks (TaskBackend)
import qualified Data.Tasks as T
import Data.Types
import Data.Utility

type HandlerM = ReaderT Config S.ActionM
type Handler = HandlerM ()

data RedisConfig = RedisConfig
  { redisHost :: Text
  , redisPort :: Int
  }

data RabbitConfig = RabbitConfig
  { rabbitHost :: Text
  , rabbitPort :: Int
  , rabbitUsername :: Text
  , rabbitPassword :: Text
  }

data PostgresConfig = PostgresConfig
  { postgresHost :: Text
  , postgresPort :: Int
  , postgresUsername :: Text
  , postgresPassword :: Text
  }

data ConfigSection = ConfigSection
  { redisSettings :: RedisConfig
  , rabbitSettings :: RabbitConfig
  , postgresSettings :: PostgresConfig
  }

data ConfigSettings = ConfigSettings
  { production :: ConfigSection
  , development :: ConfigSection
  }

data Config = Config
  { redisConnectionPool    :: Pool Redis.Connection
  , postgresConnectionPool :: Pool Postgres.Connection
  , rabbitConnectionPool   :: Pool Rabbit.Connection
  , taskBackend            :: TaskBackend BackendM
  --, logger :: Logger l
  }

data Logger m = Logger
  { logSuccess :: Text -> m ()
  , logWarn    :: Text -> m ()
  , logInfo    :: Text -> m ()
  , logError   :: Text -> m ()
  , logFatal   :: Text -> m ()
  }

data Environment = Development | Production


getConfig :: IO Config
getConfig = do
  desiredEnv <- lookupEnv "SUBMARINE"
  chosenEnv <- case desiredEnv of
    Nothing -> do
      putStrLn "No environment specified in the SUBMARINE environment variable. Defaulting to development."
      return Development
    Just str -> if str == "production"
      then return Production
      else return Development
  redisSettings <- redisPool Redis.defaultConnectInfo
  return $ Config
    { redisConnectionPool = redisSettings
    , postgresConnectionPool = undefined
    , rabbitConnectionPool = undefined
    }

poolDefaults :: IO a -> (a -> IO ()) -> IO (Pool a)
poolDefaults create destroy = createPool create destroy 4 5 200

redisPool :: Redis.ConnectInfo -> IO (Pool Redis.Connection)
redisPool connectionSettings = poolDefaults (Redis.connect connectionSettings) destroyRedisConnection
  where
    destroyRedisConnection :: Redis.Connection -> IO ()
    destroyRedisConnection c = Redis.runRedis c Redis.quit >> return ()

postgresPool :: Pool a
postgresPool = undefined

amqpPool :: Pool a
amqpPool = undefined

type BackendM = ReaderT Config IO

createTask :: NewTask -> BackendM (Id Task, FullTask)
createTask n = do
  b <- taskBackend <$> ask
  T.createTask b $ n

getTask :: Id Task -> BackendM (Maybe FullTask)
getTask i = do
  b <- taskBackend <$> ask
  T.getTask b $ i

listTasks :: T.TaskQuery -> BackendM [(Id Task, FullTask)]
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
