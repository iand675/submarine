module Submarine.Data.Redis where
import Database.Redis (Reply)
import Database.Redis.Simple (Redis)

class RedisBacked m where
	redis :: Redis a -> m (Either Reply a)

initializeRedisConnectionPool :: RedisConfig -> Foo
initializeRedisConnectionPool conf = Redis.connect $ defaultConnectInfo
	{ connectHost = unpack $ redisConfigHost conf
	, connectPort = PortNumber $ fromIntegral $ redisConfigPort conf
	}