module Submarine.Data.Redis where
import Data.Text (unpack)
import Database.Redis (Reply, Connection, ConnectInfo(..), PortID(..), connect, defaultConnectInfo)
import Database.Redis.Simple (Redis)
import Submarine.Data.Config

class RedisBacked m where
	redis :: Redis a -> m (Either Reply a)

initializeRedisConnectionPool :: RedisConfig -> IO Connection
initializeRedisConnectionPool conf = connect $ defaultConnectInfo
	{ connectHost = unpack $ redisConfigHost conf
	, connectPort = PortNumber $ fromIntegral $ redisConfigPort conf
	}