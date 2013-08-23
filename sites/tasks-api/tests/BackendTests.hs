module BackendTests where
import Distribution.TestSuite (Test)
import Test.Hspec
import TestSuiteAdapter

tests :: IO [Test]
tests = return $ specToTests $ do
  describe "task" $ do
    describe "create" $ do
      it "requires a name" $ pending
      it "returns a task that is marked incomplete" $ pending
      it "returns an identifier that can be used to retrieve the task" $ pending
    describe "get" $ do
      describe "only returns a task that a user has access to" $ do
        it "returns a task from a list that the user owns." $ False
        it "returns a task from a list that an organization owns that the user is a member of." $ pending
    describe "list" $ return ()
    describe "update" $ return ()
    describe "delete" $ return ()
  describe "list" $ do
    describe "create" $ return ()
    describe "get" $ return ()
    describe "list" $ return ()
    describe "update" $ return ()
    describe "delete" $ return ()
  describe "category" $ do
    describe "create" $ return ()
    describe "get" $ return ()
    describe "list" $ return ()
    describe "update" $ return ()
    describe "delete" $ return ()
  describe "team" $ do
    describe "create" $ return ()
    describe "get" $ return ()
    describe "list" $ return ()
    describe "update" $ return ()
    describe "delete" $ return ()
  describe "organization" $ do
    describe "create" $ return ()
    describe "get" $ return ()
    describe "list" $ return ()
    describe "update" $ return ()
    describe "delete" $ return ()
  describe "user" $ do
    describe "create" $ return ()
    describe "get" $ return ()
    describe "list" $ return ()
    describe "update" $ return ()
    describe "delete" $ do
      it "marks the user as deleted." $ pending
    describe "authenticate" $ do
      it "returns a user when supplied correct credentials." $ pending
      it "returns nothing when supplied bad credentials." $ pending
