module GitHub.Git.Trees where
import GitHub.Internal

trees o r = ownerRepo o r <> "/git/trees"
tree o r s = trees o r <> "/" <> s
--| GET /repos/:owner/:repo/git/trees/:sha
getTree ::
	OwnerName ->
	RepoName ->
	Sha ->
	GitHub TreeData
getTree o r s = get [] $ tree o r s

--| GET /repos/:owner/:repo/git/trees/:sha
getTreeRecursively ::
	OwnerName ->
	RepoName ->
	Sha ->
	GitHub TreeData
getTreeRecursively o r s = get [] $ tree o r s

--| POST /repos/:owner/:repo/git/trees
createTree ::
	OwnerName ->
	RepoName ->
	NewTree ->
	GitHub TreeData
createTree o r = post [] $ trees o r
