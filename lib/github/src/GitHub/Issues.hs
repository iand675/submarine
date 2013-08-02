module GitHub.Issues where
import GitHub.Internal

issues = "/issues"
currentUserIssues = currentUser <> issues
orgIssues o = "/orgs/" <> o <> issues
repoIssues o r = ownerRepo o r <> issues
repoIssue o r i = repoIssues <> "/" <> i

--| GET /issues
listIssues ::
	Maybe IssueFilter ->
	Maybe IssueState ->
	Maybe [LabelName] ->
	Maybe IssueSort ->
	Maybe SortOrder ->
	Maybe UTCTime ->
	GitHub IssuesData
listIssues = ghGet [] issues

--| GET /user/issues
listUserIssues ::
	Maybe IssueFilter ->
	Maybe IssueState ->
	[LabelName] ->
	Maybe IssueSort ->
	Maybe SortOrder ->
	Maybe UTCTime ->
	GitHub IssuesData
listUsersIssues = ghGet [] currentUserIssues

--| GET /orgs/:org/issues
listOrganizationIssues ::
	Maybe IssueFilter ->
	Maybe IssueState ->
	Maybe [LabelName] ->
	Maybe IssueSort ->
	Maybe SortOrder ->
	Maybe UTCTime ->
	GitHub IssuesData
listOrganizationIssues = ghGet [] orgIssues

--| GET /repos/:owner/:repo/issues
listRepositoryIssues ::
	Maybe MilestoneFilter ->
	Maybe IssueState ->
	Maybe IssueAssignee ->
	Maybe UserName ->
	Maybe UserName ->
	[LabelName] ->
	Maybe IssueSort ->
	Maybe SortOrder ->
	Maybe UTCTime ->
	GitHub IssuesData

--| GET /repos/:owner/:repo/issues/:number
getIssue ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub IssueData
getIssue o r i = ghGet [] $ repoIssue o r i

--| POST /repos/:owner/:repo/issues
createIssue ::
	OwnerName ->
	RepoName ->
	NewIssue ->
	GitHub IssueData
createIssue o r = ghPost (repoIssues o r)

--| PATCH /repos/:owner/:repo/issues/:number
editIssue ::
	OwnerName ->
	RepoName ->
	Int ->
	IssuePatch ->
	GitHub IssueData
editIssue o r i = ghPatch (repoIssue o r i)

