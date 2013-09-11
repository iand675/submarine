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
import Submarine.Models.Task

type HandlerM = ReaderT Config S.ActionM
type Handler = HandlerM ()

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
  { redisConnectionPool    :: Redis.Connection
  , postgresConnectionPool :: Pool Postgres.Connection
  , rabbitConnectionPool   :: Pool Rabbit.Connection
  , taskBackend            :: T.TaskBackend BackendM
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
  return $ Config
    { redisConnectionPool = redisSettings
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

listTasks :: TaskQuery -> BackendM [(Id Task, FullTask)]
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
