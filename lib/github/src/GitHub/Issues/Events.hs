module GitHub.Issues.Events where
import GitHub.Internal

--| GET /repos/:owner/:repo/issues/:issue_number/events
listIssueEvents ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub IssueEventsData

--| GET /repos/:owner/:repo/issues/events
listRepositoryIssueEvents ::
	OwnerName ->
	RepoName ->
	GitHub IssuesEventsData

--| GET /repos/:owner/:repo/issues/events/:id
getIssueEvent ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub IssueEventData
