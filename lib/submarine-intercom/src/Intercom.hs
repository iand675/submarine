{-# LANGUAGE QuasiQuotes #-}
module Network.Intercom where

newtype Intercom a = Intercom { fromIntercom :: ReaderT IntercomConfig (ResourceT IO) a }
  deriving (Functor, Applicative, Monad)

runIntercom :: IntercomConfig -> Intercom a -> IO a

data UserRef = UserId Int | Email Text

listUsers = get [url| /users |]

getUser :: UserRef -> Intercom User
getUser = get [url| /users{?u*} |]

createUser :: NewUser -> Intercom User
createUser = post [url| /users |]

deleteUser :: UserRef -> Intercom User
deleteUser = delete [url| /users{?u*} |]

createNote :: UserRef -> Note -> Intercom Note
createNote = post [url| /users/notes |]

createImpression :: UserRef -> Impression -> Intercom Int
createImpression = post [url| /users/impressions |]

listMessageThreads :: UserRef -> Intercom [MessageThread]
listMessageThreads = get [url| /users/message_threads |]

getMessageThread = get [url| /users/message_threads |]

createMessageThread :: 	-- create
createMessageThread = post [url| /users/message_threads |]
	-- reply
[url| /tags |]
	-- get
	-- create
	-- update
