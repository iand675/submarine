module Router where

router = do
  categories
  lists
  organizations
  tasks
  teams
  users

categories = routes "categories" $ do
  crud "/" createCategory listCategories getCategory updateCategory deleteCategory

