{-# LANGUAGE TemplateHaskell #-}
module Submarine.Data.Config where
import Data.YAML
import Submarine.JSON

loadConfig :: FromJSON a => IO (Maybe a)
loadConfig = decodeFile "config.yml"

data RedisConfig = RedisConfig
  { redisConfigHost :: Text
  , redisConfigPort :: Int
  }

jsonize ''RedisConfig

data RabbitConfig = RabbitConfig
  { rabbitConfigHost :: Text
  , rabbitConfigPort :: Int
  , rabbitConfigUsername :: Text
  , rabbitConfigPassword :: Text
  }

jsonize ''RabbitConfig

data PostgresConfig = PostgresConfig
  { postgresConfigHost :: Text
  , postgresConfigPort :: Int
  , postgresConfigUsername :: Text
  , postgresConfigPassword :: Text
  , postgresConfigDatabase :: Text
  }

jsonize ''PostgresConfig
