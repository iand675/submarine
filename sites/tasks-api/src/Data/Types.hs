{-# LANGUAGE PolyKinds #-}
module Data.Types (
  User(..),
  List(..),
  Category(..),
  Team(..),
  Organization(..),
  Id(..),
  Task(..),
  NewTask(..),
  FullTask,
  TaskPatch,
  module Data.Functor.Identity
) where
import Data.Functor.Identity

data User = User
  { userEmail             :: Text
  , userPasswordHash      :: Text
  , userName              :: Text
  , userGithubTokens      :: [Text]
  , userPhone             :: Text
  , userTaskCreationEmail :: Text
  , userStripeInfo        :: StripeInfo
  , userFogbugzInfo       :: FogBugzInfo
  }

data List = List
  { listName     :: Text
  , listCategory :: CategoryId
  }

data Category = Category
  { categoryName :: Text
  , categoryTeam :: TeamId
  }

data Team = Team
  { teamName         :: Text
  , teamOrganization :: OrganizationId
  }

data Organization = Organization
  { organizationName :: Text
  , organizationName
  }

newtype Id a = Id String
data Task f = Task
	{ taskName :: f String
	, taskComplete :: f Bool
	}

data NewTask = NewTask
	{ newTaskName :: String
	}

type FullTask = Task Identity
type TaskPatch = Task Maybe

