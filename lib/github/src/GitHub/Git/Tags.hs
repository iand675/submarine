module GitHub.Git.Tags where
import GitHub.Internal

tags o r = ownerRepo o r <> "/git/tags"
tag o r s = tags o r <> "/" <> s

--| GET /repos/:owner/:repo/git/tags/:sha
getTag ::
	OwnerName ->
	RepoName ->
	Sha ->
	GitHub TagData
getTag o r s = get [] $ tag o r s

--| POST /repos/:owner/:repo/git/tags
createTag ::
	OwnerName ->
	RepoName ->
	NewTag ->
	GitHub TagData
createTag o r = post [] $ tags o r
