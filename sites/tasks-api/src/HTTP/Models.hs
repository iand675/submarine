{-# LANGUAGE TemplateHaskell #-}
module HTTP.Models where
import Control.Monad
import Data.Aeson
import Data.Text (Text, unpack)
import Data.UUID
import Webby.JSON

instance ToJSON UUID where
	toJSON = toJSON . toString

instance FromJSON UUID where
	parseJSON (String t) = case fromString $ unpack t of
		Nothing -> fail "Invalid UUID"
		Just u -> return u
	parseJSON _ = fail "Expected JSON string"

data NewTask = NewTask
  { newTaskId :: Maybe UUID
  , newTaskName :: Text
	}

jsonize ''NewTask

data Task = Task
  { taskId :: UUID
  , taskName :: Text
  , taskComplete :: Bool
  }

jsonize ''Task

data Tasks = Tasks
	{ tasksTasks :: [Task]
	}

jsonize ''Tasks