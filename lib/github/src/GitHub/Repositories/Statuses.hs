module GitHub.Repositories.Statuses where
import GitHub.Internal

statuses o r s = ownerRepo o r <> "/statuses/" <> s

--| GET /repos/:owner/:repo/statuses/:ref
listStatuses ::
	OwnerName ->
	RepoName ->
	Ref ->
	GitHub StatusesData
listStatuses o r s = ghGet [] $ statuses o r s

--| POST /repos/:owner/:repo/statuses/:sha
createStatus ::
	OwnerName ->
	RepoName ->
	Sha ->
	NewStatus ->
	GitHub StatusData
createStatus o r s = ghPost (statuses o r s)
