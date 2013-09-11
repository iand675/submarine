{-# LANGUAGE QuasiQuotes #-}
module GitHub.Activity.Events where
import GitHub.Internal

events = "/events"
repoEvents o r = ownerRepo o r <> events
issueEvents o r = ownerRepo o r <> "/issues" <> events
networkEvents o r = [uri| /networks/{o}/{r}/events |]
orgEvents o = [uri|/orgs/{o}/events |]
users u = [uri| /users/{u} |]

-- | GET /events
listPublicEvents ::
	GitHub (Request EventsData)
listPublicEvents = get events

-- | GET /repos/:owner/:repo/events
listRepositoryEvents ::
	OwnerName ->
	RepoName ->
	GitHub EventsData
listRepositoryEvents o r = get $ repoEvents o r

-- | GET /repos/:owner/:repo/issues/events
listIssueEvents ::
	OwnerName ->
	RepoName ->
	GitHub EventsData
listIssueEvents o r = get $ issueEvents o r

-- | GET /networks/:owner/:repo/events
listRepositoryNetworkEvents ::
	OwnerName ->
	RepoName ->
	GitHub EventsData
listRepositoryNetworkEvents o r = get $ networkEvents o r

-- | GET /orgs/:org/events
listPublicOrganizationEvents ::
	OrgName ->
	GitHub EventsData
listPublicOrganizationEvents o = get $ orgEvents o

-- | GET /users/:user/received_events
listUserReceivedEvents ::
	UserName ->
	GitHub EventsData
listUserReceivedEvents u = get (users u <> "/received_events")

-- | GET /users/:user/received_events/public
listPublicUserReceivedEvents ::
	UserName ->
	GitHub EventsData
listPublicUserReceivedEvents u = get (user u <> "/received_events/public")

-- | GET /users/:user/events
listUserPerformedEvents ::
	UserName ->
	GitHub EventsData
listUserPerformedEvents u = get (user u <> events)

-- | GET /users/:user/events/public
listPublicUserPerformedEvents ::
	UserName ->
	GitHub EventsData
listPublicUserPerformedEvents = get (user u <> events <> "/public")

-- | GET /users/:user/events/orgs/:org
listUserOrganizationEvents ::
	UserName ->
	OrgName ->
	GitHub EventsData
listUserOrganizationEvents = undefined -- TODO TODO TODO

