module Tests where
import Test.Hspec

-- account
createUser = do
	describe "validation" $ do
		it "should should require the TOS checkbox is checked"
		it "should display a message if the username is taken"
		it "should display a message if the email is taken"
		it "should require all text fields be filled in"

signIn = do
	describe "submit invalid credentials" $ do
		it "should display a message that sign-in has failed"
	describe "enter empty username or password" $ do
		it "should disable form submission"
	describe "enter valid credentials" $ do
		it "should set the auth cookie"
		it "should navigate to the to-do list"

--linkFacebookAccount
--linkGitHubAccount
--linkGoogleAccount

--createGroup = do
--	return ()

-- basics
createList = do
	describe "create list button" $ do
		it "should display a form when the button is clicked" $ do
			pending "webdriver"
	describe "create list form" $ do
		it "should provide a name field for the list name" pending
		it "should not allow list creation when the name field is empty"
		it "should create the list when enter is hit"
		it "should create the list when the create button is hit"
	describe "create list" $ do
		it "should add the list to the lists section"
		it "should select the list in the lists section"

editList = do
	describe "edit list button" $ do
		it "should display a form when the button is clicked"
	describe "edit list form" $ do
		it "should initially be populated with the list's current details"
		it "should not allow saving list changes when the name field is empty"
	describe "save list edits" $ do
		it "should update the appropriate list details on the page"

archiveList = do
	describe "archive list button" $ do
		it "should confirm that the list should be archived on click"
	describe "archive list" $ do
		it "should remove the list from the lists section"
		it "should appear in the archived lists section"

createTask = do
	describe "create task button" $ do
		it "should add an empty task in edit mode to the active list when clicked" pending
	describe "new empty task" $ do
		it "should start in edit mode"
		it "should save the task changes when save is clicked"
		it "should save the task changes when focus is lost from one of the task edit fields"
		it "should not be marked as complete"

type Focus = [Tape h i Task]
moveFocus = do
	describe "move up" $ do
		it "should set the focus state to the lower-rightmost recursive child of the previous sibling of each tape if it exists"
		it "should set the focus state to the previous sibling of each tape if the previous has no children"
		it "should set the focus state to the parent if there is no previous sibling"
		it "should not change focus if there is no parent and no previous siblings"
	describe "move down"
		it "should set the focus state to its first child if it has children"
		it "should set the focus state to its subsequent sibling if it has siblings and no children"
		it "should set the focus state to its parent's subsequent sibling if it has no subsequent siblings or children"
		it "should not change focus if it has no parent or its parent has no subsequent siblings"
	describe "move left"
		it "should set the focus state to its parent if it has one"
		it "should not change focus if it has no parent"
	describe "move right"
		it "should set the focus state to its children if they exist"
		it "should not change focus if it has no children"

moveTask = do
	describe "move task up" $ do
		it "should do nothing if it is at the start of the current level"
		it "should swap the location of the previous task and the current task"
	describe "move task down" $ do
		it "should do nothing if it is at the end of the current level"
		it "should swap the location of the current task and the subsequent task"
	describe "make task a child of previous task" $ do
		it "should do nothing if there is no previous task at the current level"
		it "should be inserted at the end of the child level of the previous task"
	describe "make task's parent the parent of the current parent task" $ do
		it "should do nothing if at the root level"
		it "should be inserted at the index n+1 of the parent level where n is the index of the current parent"

moveTaskUp :: h :> TaskTree -> h :> TaskTree
moveTaskUp t = case before of
	Nothing -> t
	(Just v) -> (focus t .~ v) . (t & leftward .~ focus t)
	where before

archiveTasks = do
	describe "archive tasks button" $ do
		it "should change the state of all completed tasks in the current list to archived"
		it "should remove the all of the completed tasks in the current list"

deleteTask = do
	describe "delete task" $ do
		it "should remove the task and child tasks from the current list"

completeTask = do
	describe "clicking the task checkbox" $ do
		it "should mark task and recursive child tasks as completed"
