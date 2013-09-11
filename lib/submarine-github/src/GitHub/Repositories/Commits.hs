module GitHub.Repositories.Commits where
import GitHub.Internal

--| GET /repos/:owner/:repo/commits
listRepositoryCommits ::
	OwnerName ->
	RepoName ->
	Maybe Sha ->
	Maybe Path ->
	Maybe Author ->
	Maybe UTCTime ->
	Maybe UTCTime ->
	GitHub CommitsData

--| GET /repos/:owner/:repo/commits/:sha
getCommit ::
	OwnerName ->
	RepoName ->
	Sha ->
	GitHub CommitData

--| GET /repos/:owner/:repo/compare/:base...:head
compareCommits ::
	OwnerName ->
	RepoName ->
	GitHub DiffData
