module GitHub.Issues.Comments where
import GitHub.Internal

issuesComments o r = ownerRepo o r <> "/issues/comments"
issuesComment o r i = issuesComments o r <> "/" <> i
issueComments o r i = ownerRepo o r <> "/issues/" <> i <> "/comments"

--| GET /repos/:owner/:repo/issues/:number/comments
listIssueComments ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub IssueCommentsData
listIssueComments o r i = get [] $ issueComments o r i

--| GET /repos/:owner/:repo/issues/comments
listRepositoryIssuesComments ::
	OwnerName ->
	RepoName ->
	Maybe CommentSort ->
	Maybe SortDirection ->
	Maybe UTCTime ->
	GitHub IssueCommentsData
listRepositoryIssuesComments o r = get [] $ issuesComments o r

--| GET /repos/:owner/:repo/issues/comments/:id
getIssueComment ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub IssueCommentData
getIssueComment o r i = get [] $ issuesComment o r i

--| POST /repos/:owner/:repo/issues/:number/comments
createIssueComment ::
	OwnerName ->
	RepoName ->
	Int ->
	NewIssueComment ->
	GitHub IssueCommentData
createIssueComment o r i = post [] $ issuesComment o r i

--| PATCH /repos/:owner/:repo/issues/comments/:id
editIssueComment ::
	OwnerName ->
	RepoName ->
	IssueCommentPatch ->
	GitHub IssueCommentData
editIssueComment o r i = patch [] $ issuesComment o r i

--| DELETE /repos/:owner/:repo/issues/comments/:id
deleteIssueComment ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub ()
deleteIssueComment o r i = delete [] $ issuesComment o r i
