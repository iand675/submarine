module Submarine.Data.Lists where

data ListBackend m = ListBackend
  { createList :: NewList -> m (Id List, FullList)
  , getList    :: Id List -> m (Maybe List)
  , updateList :: Id List -> ListPatch -> m (Maybe List)
  , deleteList :: Id List -> m (Maybe List)
  , listLists  :: ListQuery -> m [(Id List, FullList)]
  }
