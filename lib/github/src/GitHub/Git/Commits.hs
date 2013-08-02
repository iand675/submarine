module GitHub.Git.Commits where
import GitHub.Internal

commits o r = ownerRepo o r <> "/commits"
commit o r s = ownerRepo o r <> "/" <> s
--| GET /repos/:owner/:repo/git/commits/:sha
getCommit ::
	OwnerName ->
	RepoName ->
	Sha ->
	GitHub CommitData
getCommit o r = get [] $ commit o r

--| POST /repos/:owner/:repo/git/commits
createCommit ::
	OwnerName ->
	RepoName ->
	NewCommit ->
	GitHub CommitData
createCommit o r = post $ commits o r
