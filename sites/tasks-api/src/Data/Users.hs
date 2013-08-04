module Data.Users where

data UserBackend m = UserBackend
  { createUser :: NewUser -> m (Id User, FullUser)
  , getUser    :: Id User -> m (Maybe FullUser)
  , updateUser :: Id User -> UserPatch -> m (Maybe FullUser)
  , deleteUser :: Id User -> m (Maybe FullUser)
  , listUsers  :: UserQuery -> m [(Id User, FullUser)]
  }

