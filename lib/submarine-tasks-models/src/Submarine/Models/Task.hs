{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Submarine.Models.Task where
import Data.Text (Text)
import Data.UUID (UUID)

import Submarine.Models.Accounts
import Submarine.Common.Models
import Submarine.JSON

data Organization = Organization
  { organizationName :: Text
  }
type instance Id Organization = Int
type OrganizationId = Id Organization
jsonize ''Organization

data Team = Team
  { teamName         :: Text
  , teamOrganization :: OrganizationId
  }
type instance Id Team = Int
type TeamId = Id Team
jsonize ''Team

data Category = Category
  { categoryName :: Text
  , categoryTeam :: TeamId
  }
type instance Id Category = Int
type CategoryId = Id Category
jsonize ''Category

data List = List
  { listName     :: Text
  , listCategory :: CategoryId
  }
type NewList = List
type instance Id List = UUID
jsonize ''List

data Task = Task
	{ taskName :: Text
	, taskComplete :: Bool
	}
type TaskPatch = Task
type FullTask = Task
type instance Id Task = UUID
jsonize ''Task

data NewTask = NewTask
	{ newTaskName :: Text
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
