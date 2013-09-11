module GitHub.Activity.Starring where
import GitHub.Internal

data Direction = Ascending | Descending
data StarredRepositoriesSortBy = Created | Updated

--| GET /repos/:owner/:repo/stargazers
listStargazers ::
	Username ->
	Reponame ->
	Maybe StarredRepositoriesSortBy ->
	Maybe Direction
	GitHub StargazersData

--| GET /users/:user/starred
listStarredRepositories ::
	Username ->
	Maybe StarredRepositoriesSortBy
	Maybe Direction
	GitHub StarredRepositoriesData

--| GET /user/starred
listCurrentUserStarredRepositories ::
	Maybe StarredRepositoriesSortBy
	Maybe Direction
	GitHub StarredRepositoriesData

--| GET /user/starred/:owner/:repo
isRepositoryStarred ::
	Username ->
	Reponame ->
	GitHub Bool

--| PUT /user/starred/:owner/:repo
starRepository ::
	Username ->
	Reponame ->
	GitHub ()

--| DELETE /user/starred/:owner/:repo
unstarRepository ::
	Username ->
	Reponame ->
	GitHub ()
