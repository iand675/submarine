module GitHub.Activity.Notifications where
import GitHub.Internal

notifications = "/notifications"
thread i = notifications <> "/threads/" <> i
subscription = thread i <> "/subscription"
repoNotifications o r = ownerRepo o r <> "/notifications"

mParam :: IsString s => s -> Maybe s -> Maybe (s, s)

-- GET /notifications
listNotifications ::
	Maybe Bool ->
	Maybe Bool ->
	Maybe UTCTime ->
	GitHub NotificationData
listNotifications a p s = ghGet
	[ mParam "all" a
	, mParam "participating" p
	, mParam "since" s
	]
	$ notifications

-- GET /repos/:owner/:repo/notifications
listRepositoryNotifications ::
	OwnerName ->
	RepoName ->
	Maybe Bool ->
	Maybe Bool ->
	Maybe UTCTime ->
	GitHub NotificationData
listRepositoryNotifications o r a p s = ghGet
	[ mParam "all" a
	, mParam "participating" p
	, mParam "since" s
	]
	$ repoNotifications o r

-- PUT /notifications
markNotificationsRead ::
	Maybe UTCTime ->
	GitHub NotificationData
markNotificationsRead o r s = ghPut' notifications []

-- PUT /repos/:owner/:repo/notifications
markRepositoryNotificationsRead ::
	OwnerName ->
	RepoName ->
	Maybe UTCTime ->
	GitHub NotificationData
markRepositoryNotificationsRead o r t = ghPut (repoNotifications o r) $ NotificationsReadOptions t

-- GET /notifications/threads/:id
getNotificationThread ::
	Int ->
	GitHub NotificationThreadData
getNotificationThread = ghGet [] . thread

-- PATCH /notifications/threads/:id
markNotificationThreadRead ::
	Int ->
	GitHub ()
markNotificationThreadRead = ghPatch' . thread

-- GET /notifications/threads/:id/subscription
getThreadSubscription ::
	Int ->
	GitHub ThreadSubscriptionData
getThreadSubscription = ghGet [] . subscription

-- PUT /notifications/threads/:id/subscription
setThreadSubscription ::
	Int ->
	NewSubscription ->
	GitHub ThreadSubscriptionData
setThreadSubscription i = ghPut (subscription i)

-- DELETE /notifications/threads/:id/subscription
deleteThreadSubscription ::
	Int ->
	GitHub ()
deleteThreadSubscription = ghDelete . subscription
