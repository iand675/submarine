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
import Data.Text (Text)
