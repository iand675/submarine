module GitHub.Orgs.Members where

org o = "/orgs/" <> o
members o = orgs o <> "/members"
member o u = members o <> "/" <> u
publicMembers o = orgs o <> "/public_members"
publicMember o u = publicMembers o <> "/" <> u

--| GET /orgs/:org/members
listOrgMembers ::
	OrgName ->
	GitHub OrgMembersData
listOrgMembers = get [] . members

--| GET /orgs/:org/members/:user
getOrgMembershipStatus ::
	OrgName ->
	UserName ->
	GitHub Bool
getOrgMembershipStatus o u = get [] $ member o u

--| DELETE /orgs/:org/members/:user
removeMember ::
	OrgName ->
	UserName ->
	GitHub ()
removeMember o u = delete [] $ members o u

--| GET /orgs/:org/public_members
listPublicOrgMembers ::
	OrgName ->
	GitHub OrgMembersData
listPublicOrgMembers = get [] . publicMembers

--| GET /orgs/:org/public_members/:user
getOrgPublicMembershipStatus ::
	OrgName ->
	UserName ->
	GitHub Bool
getOrgPublicMembershipStatus o u = get [] $ publicMember o u

--| PUT /orgs/:org/public_members/:user
publicizeOrgMembership ::
	OrgName ->
	UserName ->
	GitHub ()
publicizeOrgMembership o u = post [] $ publicMember o u

--| DELETE /orgs/:org/public_members/:user
concealOrgMembership ::
	OrgName ->
	UserName ->
	GitHub ()
concealOrgMembership o u = delete [] $ publicMember o u
