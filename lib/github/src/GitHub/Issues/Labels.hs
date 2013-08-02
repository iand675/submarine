module GitHub.Issues.Labels where
import GitHub.Internal

--| GET /repos/:owner/:repo/labels
listRepositoryLabels ::
	OwnerName ->
	RepoName ->
	GitHub LabelsData

--| GET /repos/:owner/:repo/labels/:name
getLabel ::
	OwnerName ->
	RepoName ->
	GitHub LabelData

--| POST /repos/:owner/:repo/labels
createLabel ::
	OwnerName ->
	RepoName ->
	NewLabel ->
	GitHub LabelData

--| PATCH /repos/:owner/:repo/labels/:name
updateLabel ::
	OwnerName ->
	RepoName ->
	Text ->
	Color ->
	GitHub LabelData

--| DELETE /repos/:owner/:repo/labels/:name
deleteLabel ::
	OwnerName ->
	RepoName ->
	Text ->
	GitHub ()

--| GET /repos/:owner/:repo/issues/:number/labels
listIssueLabels ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub LabelsData

--| POST /repos/:owner/:repo/issues/:number/labels
addIssueLabels ::
	OwnerName ->
	RepoName ->
	[Text] ->
	GitHub LabelsData

--| DELETE /repos/:owner/:repo/issues/:number/labels/:name
removeIssueLabel ::
	OwnerName ->
	RepoName ->
	GitHub ()

--| PUT /repos/:owner/:repo/issues/:number/labels
setIssueLabels ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub LabelsData

--| DELETE /repos/:owner/:repo/issues/:number/labels
deleteIssueLabels ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub ()

--| GET /repos/:owner/:repo/milestones/:number/labels
listMilestoneLabels ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub LabelsData

