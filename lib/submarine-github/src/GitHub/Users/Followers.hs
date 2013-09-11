module GitHub.Users.Followers where
import GitHub.Internal

followers = "/followers"
following = "/following"
userFollowers u = user u <> followers
currentUserFollowers = currentUser <> followers
userFollowing u = user u <> following
currentUserFollowing = currentUser <> following
currentUserFollowingUser u = currentUserFollowing <> "/" <> u

--| GET /users/:user/followers
listFollowers ::
	UserName ->
	GitHub FollowersData
listFollowers = ghGet . userFollowers

--| GET /users/followers
listCurrentUserFollowers ::
	GitHub FollowersData
listCurrentUserFollowers = ghGet currentUserFollowers

--| GET /users/:user/following
listFollowing ::
	UserName ->
	GitHub FollowersData
listFollowing = ghGet . userFollowing

--| GET /user/following
listCurrentUserFollowing ::
	GitHub FollowersData
listCurrentUserFollowing = ghGet currentUserFollowing

--| GET /user/following/:user
isCurrentUserFollowing ::
	UserName ->
	GitHub Bool
isCurrentUserFollowing u = ghGet ("/user/following/" <> u)

--| GET /users/:user/following/:target_user
isUserFollowing ::
	UserName ->
	UserName ->
	GitHub Bool
isUserFollowing u u' = ghGet (userFollowing u <> "/" <> u')

--| POST /user/following/:user
followUser ::
	UserName ->
	GitHub ()
followUser = ghPost' . currentUserFollowingUser

--| DELETE /user/following/:user
unfollowUser ::
	UserName ->
	GitHub ()
unfollowUser = ghDelete . currentUserFollowingUser
