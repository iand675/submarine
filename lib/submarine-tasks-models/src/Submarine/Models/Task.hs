{-# LANGUAGE TemplateHaskell #-}
module Submarine.Models.Task where
import Data.Text (Text)

import Submarine.Models.Accounts
import Submarine.Common.Models
import Submarine.JSON

data TaskQuery = Where [QueryPredicate]

data QueryPredicate
  = AssignedTo (Id User)
  | CreatedBy  (Id User)
  | ListIs     (Id List)
  | CategoryIs (Id Category)
  | HasTags    [Text]

data List = List
  { listName     :: Text
  , listCategory :: CategoryId
  }

jsonize ''List

type CategoryId = Id Category
data Category = Category
  { categoryName :: Text
  , categoryTeam :: TeamId
  }

jsonize ''Category

type TeamId = Id Team
data Team = Team
  { teamName         :: Text
  , teamOrganization :: OrganizationId
  }

jsonize ''Team

type OrganizationId = Id Organization
data Organization = Organization
  { organizationName :: Text
  }

jsonize ''Organization

data Task f = Task
	{ taskName :: f String
	, taskComplete :: f Bool
	}

jsonize ''Task

data NewTask = NewTask
	{ newTaskName :: String
	}

jsonize ''NewTask

type FullTask = Task Identity

type TaskPatch = Task Maybe

data TaskCreated = TaskCreated
  { taskCreatedId :: Id Task
  -- , taskCreatedTask :: Task Identity
  }

jsonize ''TaskCreated
