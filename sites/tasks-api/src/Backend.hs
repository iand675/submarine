{-# LANGUAGE PolyKinds #-}
module Backend where
import AMQP
import Database.Redis.Simple
import Database.PostgreSQL.Simple

type FullTask = Task Identity
type TaskPatch = Task Maybe

createTask :: Monad m => NewTask -> m (Id Task, FullTask)
updateTask :: Monad m => Id Task -> TaskPatch -> m (Maybe FullTask)
deleteTask :: Monad m => Id Task -> m (Maybe FullTask)
