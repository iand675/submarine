module GitHub.Issues.Milestones where
import GitHub.Internal

milestones o r = ownerRepo o r <> "/milestones"
milestone o r i = milestones o r <> "/" <> i
--| GET /repos/:owner/:repo/milestones
listRepositoryMilestones ::
	OwnerName ->
	RepoName ->
	Maybe MilestoneState ->
	Maybe MilestoneSort ->
	Maybe SortDirection ->
	GitHub MilestonesData
listRepositoryMilestones = get [] $ milestones o r i

--| GET /repos/:owner/:repo/milestones/:number
getRepositoryMilestone ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub MilestoneData
getRepositoryMilestone = get [] $ milestone o r i

--| POST /repos/:owner/:repo/milestones
createMilestone ::
	OwnerName ->
	RepoName ->
	NewMilestone ->
	GitHub MilestoneData
createMilestone = post [] $ milestones o r

--| PATCH /repos/:owner/:repo/milestones/:number
updateMilestone ::
	OwnerName ->
	RepoName ->
	Int ->
	MilestonePatch ->
	GitHub MilestoneData
updateMilestone = patch [] $ milestone o r i

--| DELETE /repos/:owner/:repo/milestones/:number
deleteMilestone ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub ()
deleteMilestone = delete [] $ milestone o r i
