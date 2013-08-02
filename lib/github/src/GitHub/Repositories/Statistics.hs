module GitHub.Repositories.Statistics where
import GitHub.Internal

stats o r e = ownerRepo o r <> "/stats/" <> e

--| GET /repos/:owner/:repo/stats/contributors
getContributorsStatistics ::
	OwnerName ->
	RepoName ->
	GitHub ContributorStatisticsData
getContributorsStatistics o r = ghGet [] $ stats o r "contributors"

--| GET /repos/:owner/:repo/stats/commit_activity
getPastYearCommitActivityStatistics ::
	OwnerName ->
	RepoName ->
	GitHub CommitActivityStatisticsData
getPastYearCommitActivityStatistics o r = ghGet [] $ stats o r "commit_activity"

--| GET /repos/:owner/:repo/stats/code_frequency
getWeeklyCodeFrequencyStatistics ::
	OwnerName ->
	RepoName ->
	GitHub CodeFrequencyStatisticsData
getWeeklyCodeFrequencyStatistics o r = ghGet [] $ stats o r "code_frequency"

--| GET /repos/:owner/:repo/stats/participation
getWeeklyParticipationStatistics ::
	OwnerName ->
	RepoName ->
	GitHub ParticipationStatisticsData
getWeeklyParticipationStatistics o r = ghGet [] $ stats o r "participation"

--| GET /repos/:owner/:repo/stats/punch_card
getDailyPunchCardStatistics ::
	OwnerName ->
	RepoName ->
	GitHub PunchCardStatisticsData
getDailyPunchCardStatistics o r = ghGet [] $ stats o r "punch_card"
