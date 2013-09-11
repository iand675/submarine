module GitHub.Orgs where
import GitHub.Internal

orgs = "/orgs"
org o = orgs <> "/" <> o
currentUserOrgs = currentUser <> "/" <> orgs
userOrgs u = user u <> "/" <> orgs

--| GET /users/:user/orgs
listUserOrganizations ::
	UserName ->
	GitHub OrgsData
listUserOrganizations u = ghGet . user

--| GET /user/orgs
listCurrentUserOrganizations ::
	GitHub OrgsData
listUserOrganizations = ghGet currentUserOrgs

--| GET /orgs/:org
getOrganization ::
	OrgName ->
	GitHub OrgData
getOrganization = ghGet . org

--| PATCH /orgs/:org
editOrganization ::
	OrgName ->
	OrgPatch
	GitHub OrgData
editOrganization o = ghPatch (org o)



