module GitHub.PullRequests.ReviewComments where
import GitHub.Internal

--| GET /repos/:owner/:repo/pulls/:number/comments
listPullRequestComments ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub ReviewComments

listRepoPullRequestComments ::
	OwnerName ->
	RepoName ->
	ReviewCommentSortBy ->
	SortOrder ->
	UTCTime ->
	GitHub ReviewComments

getPullRequestComment ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub ReviewComment

createPullRequestComment ::
	OwnerName ->
	RepoName ->
	Int ->
	ReviewComment ->
	GitHub ReviewComment

editPullRequestComment ::
	OwnerName ->
	RepoName ->
	Int ->
	ReviewCommentPatch ->
	GitHub ReviewComment

deletePullRequestComment ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub ()
