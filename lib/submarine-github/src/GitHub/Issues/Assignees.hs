module GitHub.Issues.Assignees where
import GitHub.Internal

assignees o r = ownerRepo o r <> "/assignees"
assignee o r a = assignees o r <> "/" <> a

--| GET /repos/:owner/:repo/assignees
listAssignees ::
	OwnerName ->
	RepoName ->
	GitHub AssigneesData
listAssignees o r = get [] $ assignees o r

--| GET /repos/:owner/:repo/assignees/:assignee
checkAssignee ::
	OwnerName ->
	RepoName ->
	UserName ->
	GitHub Bool
checkAssignee o r u = get [] $ assignee o r u
