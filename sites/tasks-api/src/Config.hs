module Config where
import Data.Pool
import Database.Redis

data Config l = Config
  { redisConnectionPool    :: Pool Redis.Connection
  , postgresConnectionPool :: Pool Postgres.Connection
  , rabbitConnectionPool   :: Pool Rabbit.Connection
  , logger :: Logger l
  }

data Logger m = Logger
  { logSuccess :: Text -> m ()
  , logWarn    :: Text -> m ()
  , logInfo    :: Text -> m ()
  , logError   :: Text -> m ()
  , logFatal   :: Text -> m ()
  }

data Environment = Development | Production

getConfig = IO Config
getConfig = ("SUBMARINE_ENVIRONMENT" !) <$> getEnvironment >>= lookup environmentSettings

poolDefaults :: IO a -> (a -> IO ()) -> Pool a
poolDefaults create destroy = createPool create destroy 4 5 200

redisPool :: Pool Redis.Connection
redisPool connectionSettings = poolDefaults (connect connectionSettings) (\c -> runRedis c quit)

postgresPool :: Pool a

amqpPool :: Pool a

