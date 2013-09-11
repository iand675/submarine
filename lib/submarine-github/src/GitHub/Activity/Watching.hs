module GitHub.Activity.Watching where
import GitHub.Internal

subscription u r = ownedRepo u r <> "/subscription"

--| GET /repos/:owner/:repo/subscribers
listWatchers ::
	OwnerName ->
	RepoName ->
	GitHub WatchersData
listWatchers u r = ghGet (ownedRepo u r <> "/subscribers")

--| GET /users/:user/subscriptions
listWatchedRepositories ::
	UserName ->
	GitHub WatchedRepoData

--| GET /user/subscriptions
listCurrentUserWatchedRepositories ::
	GitHub WatchedRepoData
listCurrentUserWatchedRepositories = ghGet "/user/subscriptions"

--| GET /repos/:owner/:repo/subscription
getRepositorySubscription ::
	Username ->
	Reponame ->
	GitHub RepositorySubscriptionData
getRepositorySubscription u r = ghGet $ subscription u r

--| PUT /repos/:owner/:repo/subscription
setRepositorySubscription ::
	Username ->
	Reponame ->
	RepositorySubscriptionSettings
	GitHub RepositorySubscriptionData
setRepositorySubscription u r = ghPut $ subscription u r

--| DELETE /repos/:owner/:repo/subscription
deleteRepositorySubscription ::
	Username ->
	Reponame ->
	GitHub ()
deleteRepositorySubscription u r = ghDelete $ subscription u r
