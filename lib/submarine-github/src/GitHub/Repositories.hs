module GitHub.Repositories where
import GitHub.Internal

--| GET /user/repos
listCurrentUserRepositories ::
	Maybe RepositoryType ->
	Maybe RepositorySort ->
	Maybe SortDirection ->
	GitHub RepositoriesData

--| GET /users/:user/repos
listUserRepositories ::
	Maybe RepositoryType ->
	Maybe RepositorySort ->
	Maybe SortDirection ->
	GitHub RepositoriesData

--| GET /orgs/:org/repos
listOrgRepositories ::
	Maybe OrgRepositoryType ->
	GitHub RepositoriesData

--| GET /repositories
listAllRepositories ::
	Maybe Int ->
	GitHub RepositoriesData

--| POST /user/repos
createUserRepository ::
	NewRepository ->
	GitHub RepositoryData

--| POST /orgs/:org/repos
createOrgRepository ::
	NewRepository ->
	GitHub RepositoryData

--| GET /repos/:owner/:repo
getRepository ::
	OwnerName ->
	RepoName ->
	GitHub RepositoryData

--| PATCH /repos/:owner/:repo
editRepository ::
	OwnerName ->
	RepoName ->
	RepositoryPatch ->
	GitHub RepositoryData

--| GET /repos/:owner/:repo/contributors
listContributors ::
	OwnerName ->
	RepoName ->
	Maybe Bool ->
	GitHub ContributorsData

--| GET /repos/:owner/:repo/languages
listLanguages ::
	OwnerName ->
	RepoName ->
	GitHub LanguagesData

--| GET /repos/:owner/:repo/teams
listTeams ::
	OwnerName ->
	RepoName ->
	GitHub TeamsData

--| GET /repos/:owner/:repo/tags
listTags ::
	OwnerName ->
	RepoName ->
	GitHub TagsData

--| GET /repos/:owner/:repo/branches
listBranches ::
	OwnerName ->
	RepoName ->
	GitHub BranchesData

--| GET /repos/:owner/:repo/branches/:branch
getBranch ::
	OwnerName ->
	RepoName ->
	BranchName ->
	GitHub BranchData

--| DELETE /repos/:owner/:repo
deleteRepository ::
	OwnerName ->
	RepoName ->
	GitHub ()
