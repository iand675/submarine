module Submarine.Data.Lists where
import Submarine.Models.Task
import Submarine.Common.Models

data ListBackend m = ListBackend
  { createList :: NewList -> m (Id List, List)
  , getList    :: Id List -> m (Maybe List)
  , updateList :: Id List -> List -> m (Maybe List)
  , deleteList :: Id List -> m (Maybe List)
  , listLists  :: ListQuery -> m [(Id List, List)]
  }
