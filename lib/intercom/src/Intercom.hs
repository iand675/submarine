module Network.Intercom where

listUsers
listUsers = get [url| /users |]

getUser :: Either UserId Email -> Intercom User
getUser = get [url| /users{?user_id, email} |]

createUser :: NewUser -> Intercom User
createUser = post [url| /users |]

deleteUser :: Either UserId Email -> Intercom User
deleteUser = delete [url| /users{?user_id, email} |]

createNote :: Either UserId Email -> Note -> Intercom Note
createNote = post [url| /users/notes |]

createImpression :: Either UserId Email -> Impression -> Intercom Int
createImpression = post [url| /users/impressions |]

listMessageThreads :: Either UserId Email -> Intercom [MessageThread]
listMessageThreads = get [url| /users/message_threads |]

getMessageThread = get [url| /users/message_threads |]

createMessageThread :: 	-- create
createMessageThread = post [url| /users/message_threads |]
	-- reply
[url| /tags |]
	-- get
	-- create
	-- update