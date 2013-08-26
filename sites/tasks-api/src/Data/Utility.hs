module Data.Utility where
import Database.Redis (Reply)

import AMQP (AMQP)
import Database.Redis.Simple

--class PostgresBacked m where
  --postgres :: Postgres a -> m a

class AMQPBacked m where
  amqp :: AMQP a -> m a
