module GitHub.Orgs.Teams where
import GitHub.Internal

teams = "/teams"
orgTeams o = orgs o <> teamss
team i = teams <> "/" <> i
teamRepo i o r = team i <> ownerRepo o r

--| GET /orgs/:org/teams
listTeams ::
	OrgName ->
	GitHubs TeamsData
listTeams o = get [] (org o <> "/teams")

--| GET /teams/:id
getTeam ::
	Int ->
	GitHub TeamData
getTeam = get [] $ team i

--| POST /orgs/:org/teams
createTeam ::
	OrgName ->
	NewTeam ->
	GitHub TeamData
createTeam o = post [] $ orgTeams o

--| PATCH /teams/:id
editTeam ::
	Int ->
	TeamPatch ->
	GitHub TeamData
editTeam i = patch [] $ team i

--| DELETE /teams/:id
deleteTeam ::
	Int ->
	GitHub ()
deleteTeam = delete [] . team

--| GET /teams/:id/members
listTeamMembers ::
	Int ->
	GitHub UsersData
listTeamMembers = get [] . team

--| GET /teams/:id/members/:user
getTeamMember ::
	Int ->
	UserName ->
	GitHub UserData

--| PUT /teams/:id/members/:user
addTeamMember ::
	Int ->
	UserName ->
	GitHub ()

--| DELETE /teams/:id/members/:user
removeTeamMember ::
	Int ->
	UserName ->
	GitHub ()

--| GET /teams/:id/repos
listTeamRepos ::
	Int ->
	GitHub ReposListData
listTeamRepos i = get [] (team i <> "/repos")

--| GET /teams/:id/repos/:owner/:repo
getTeamRepo ::
	Int ->
	OwnerName ->
	RepoName ->
	GitHub Bool
getTeamRepo i o r = get [] $ teamRepo i o r

--| PUT /teams/:id/repos/:org/:repo
addTeamRepo ::
	Int ->
	OwnerName ->
	RepoName ->
	GitHub ()
addTeamRepo i o r = post [] $ teamRepo i o r

--| DELETE /teams/:id/repos/:owner/:repo
removeTeamRepo ::
	Int ->
	RepoName ->
	OwnerName ->
	GitHub ()
removeTeamRepo i o r = delete [] $ teamRepo i o r
