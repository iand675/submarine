
-- REST API
-- make a call
-- call SIP-enabled endpoint
-- retrieve call logs
-- modify live calls
-- verify caller id
-- view call transcriptions
-- view call recordings
-- manage conferences
-- view conference participants
-- create call queue
-- view call queues
-- view queue members
-- dequeue member
-- send sms
-- send short code sms
-- retrieve sms logs
-- search for phone numbers
-- purchase phone numbers
-- update phone number properties
-- view data usage
-- set usage threshold triggers
-- ACCOUNTS...
-- SUBACCOUNTS...
-- APPLICATIONS...
-- CONNECT APPS
-- AUTHORIZED CONNECT APPS

data VoiceGender = Man | Woman
data VoiceLanguage = AmericanEnglish | BritishEnglish | Spanish | French | German | Italian
type RepeatCount = Int

data Key = Digit Int | Pound | Star

data Say = Say
	{ voice :: Maybe VoiceGender
	, language :: Maybe VoiceLanguage
	, loop :: Maybe RepeatCount
	, text :: Text
	}

data Play = Play
	{ loop :: Maybe RepeatCount
	, url :: Text
	}

class GatherAction a where {}
instance GatherAction Say where {}
instance GatherAction Play where {}
instance GatherAction Pause where {}

data Gather = Gather
	{ action :: Maybe Text
	, method :: Maybe Method
	, timeout :: Maybe Int
	, finishOnKey :: Maybe Key
	, numDigits :: Maybe Int
	, actions :: [SerializedVerb]
	}

data Pause = Pause
	{ _pauseLength :: Maybe Int
	}

-- Twiml
-- voice
	-- say
	-- play
	-- gather
	-- record
	-- sms
	-- dial
		-- number
		-- sip
		-- client
		-- conference
		-- queue
	-- enqueue
	-- leave
	-- hangup
	-- redirect
	-- reject
	-- pause
-- sms
	-- sms
	-- redirect