module GitHub.PullRequests where
import GitHub.Internal

pulls o r = ownerRepo o r <> "/pulls"
pull o r i = pulls o r <> "/" <> i
commits = "/commits"
files = "/files"
merge = "/merge"

--| GET /repos/:owner/:repo/pulls
listPullRequests ::
	OwnerName ->
	RepoName ->
	Maybe PullRequestState ->
	Maybe Text ->
	Maybe BaseBranchName
	GitHub PullRequestsData
listPullRequests o r s t b

--| GET /repos/:owner/:repo/pulls/:number
getPullRequest ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub PullRequestData
getPullRequest o r i = ghGet [] (pull o r i)

--| POST /repos/:owner/:repo/pulls
createPullRequest ::
	OwnerName ->
	RepoName ->
	NewPullRequest ->
	GitHub PullRequestData
createPullRequest o r = ghPost (pulls o r)

--| PATCH /repos/:owner/:repo/pulls/:number
updatePullRequest ::
	OwnerName ->
	RepoName ->
	Int ->
	PullRequestPatch ->
	GitHub PullRequestData
updatePullRequest o r i = ghPatch (pull o r i)

--| GET /repos/:owners/:repo/pulls/:number/commits
listPullRequestCommits ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub CommitsData
listPullRequestCommits o r i = ghGet [] (pull o r i <> commits)

--| GET /repos/:owners/:repo/pulls/:number/files
listPullRequestFiles ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub PullRequestFilesData
listPullRequestFiles o r i = ghGet [] (pull o r i <> files)

--| GET /repos/:owner/:repo/pulls/:number/merge
checkMergeStatus ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub MergeStatus
 checkMergeStatus o r i = ghGet [] (pull o r i <> merge)

--| PUT /repos/:owner/:repo/pulls/:number/merge
mergePullRequest ::
	OwnerName ->
	RepoName ->
	Int ->
	Maybe Text ->
	GitHub MergeData
mergePullRequest o r i = ghPut (pull o r i <> merge)
