{-# LANGUAGE PolyKinds, TemplateHaskell #-}
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
import Data.Aeson
import Data.Functor.Identity
import Data.Text (Text)
import Data.UUID

import Webby.JSON

instance (ToJSON a) => ToJSON (Identity a) where
  toJSON = toJSON . runIdentity 

instance (FromJSON a) => FromJSON (Identity a) where
  parseJSON = fmap Identity . parseJSON

type StripeInfo = ()
type FogBugzInfo = ()

data Entity k a = Entity
  { entityKey :: !(Id k)
  , entityValue :: !a
  }

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

type CategoryId = Id Category
data Category = Category
  { categoryName :: Text
  , categoryTeam :: TeamId
  }

type TeamId = Id Team
data Team = Team
  { teamName         :: Text
  , teamOrganization :: OrganizationId
  }

type OrganizationId = Id Organization
data Organization = Organization
  { organizationName :: Text
  }

newtype Id a = Id UUID

instance ToJSON UUID where
  toJSON = toJSON . toString

instance FromJSON UUID where
  parseJSON = parseJSON

instance ToJSON (Id a) where
  toJSON (Id u) = toJSON u

instance FromJSON (Id a) where
  parseJSON = fmap Id . parseJSON

data Task f = Task
	{ taskName :: f String
	, taskComplete :: f Bool
	}

data NewTask = NewTask
	{ newTaskName :: String
	}

type FullTask = Task Identity

type TaskPatch = Task Maybe

data TaskCreated = TaskCreated
  { taskCreatedId :: Id Task
  -- , taskCreatedTask :: Task Identity
  }

--jsonize ''TaskCreated
