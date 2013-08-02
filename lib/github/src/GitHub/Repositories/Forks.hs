module GitHub.Repositories.Forks where
import GitHub.Internal

forks o r = ownerRepo o r <> "/forks"

--| GET /repos/:owner/:repo/forks
listForks ::
	OwnerName ->
	RepoName ->
	Maybe ForkSort ->
	GitHub RepositoriesData
listForks o r s = ghGet [s] (forks o r)

--| POST /repos/:owner/:repo/forks
createFork ::
	OwnerName ->
	RepoName ->
	Maybe OrgName ->
	GitHub RepositoryData
createFork o r n = ghPost [n] (forks o r)
