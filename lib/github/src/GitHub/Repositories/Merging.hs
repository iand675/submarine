module GitHub.Repositories.Merging where
import GitHub.Internal

--| POST /repos/:owner/:repo/merges
performMerge ::
	OwnerName ->
	RepoName ->
	MergeInfo ->
	GitHub CommitData
performMerge o r = ghPost (ownerRepo o r)
