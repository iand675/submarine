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

data User
data List
data Category
data Team
data Organization

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

