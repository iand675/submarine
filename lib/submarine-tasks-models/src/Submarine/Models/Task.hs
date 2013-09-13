{-# LANGUAGE TemplateHaskell #-}
module Submarine.Models.Task where
import Data.Text (Text)

import Submarine.Models.Accounts
import Submarine.Common.Models
import Submarine.JSON

data Organization = Organization
  { organizationName :: Text
  }
type OrganizationId = Id Organization

jsonize ''Organization

data Team = Team
  { teamName         :: Text
  , teamOrganization :: OrganizationId
  }
type TeamId = Id Team

jsonize ''Team

data Category = Category
  { categoryName :: Text
  , categoryTeam :: TeamId
  }
type CategoryId = Id Category

jsonize ''Category

data List = List
  { listName     :: Text
  , listCategory :: CategoryId
  }
type NewList = List

jsonize ''List

data Task = Task
	{ taskName :: String
	, taskComplete :: Bool
	}
type TaskPatch = Task
type FullTask = Task

jsonize ''Task

data NewTask = NewTask
	{ newTaskName :: String
	}

jsonize ''NewTask

--type FullTask = Task Identity

--type TaskPatch = Task Maybe

data TaskCreated = TaskCreated
  { taskCreatedId :: Id Task
  -- , taskCreatedTask :: Task Identity
  }

jsonize ''TaskCreated

data TaskQuery = Where [QueryPredicate]

data QueryPredicate
  = AssignedTo (Id User)
  | CreatedBy  (Id User)
  | ListIs     (Id List)
  | CategoryIs (Id Category)
  | HasTags    [Text]

data ListQuery = ListQuery

data Tasks = Tasks { tasksTasks :: [Task] }

jsonize ''Tasks