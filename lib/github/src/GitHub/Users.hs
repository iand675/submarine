module GitHub.Users where
import GitHub.Internal

currentUser = "/user"
users = "/users"
user u = users <> "/" <> u

--| GET /users/:user
getUser ::
	UserName ->
	GitHub UserData
getUser = ghGet . user

--| GET /user
getCurrentUser ::
	GitHub CurrentUserData
getCurrentUser = ghGet currentUser

--| PATCH /user
updateCurrentUser ::
	CurrentUserPatch ->
	GitHub CurrentUserData
updateCurrentUser = ghPatch currentUser

--| GET /users
listUsers ::
	Maybe Int ->
	GitHub UsersData
listUsers = ghGet users
