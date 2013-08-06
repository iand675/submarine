module Data.Utility where

class PostgresBacked m where
	postgres :: Postgres a -> m a

class AMQPBacked m where
	amqp :: AMQP a -> m a

class RedisBacked m where
	redis :: Redis a -> m a

single :: [a] -> Maybe a
single xs = case xs of
	(x:[]) -> Just x
	_      -> Nothing

