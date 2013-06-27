module Network.Twilio.TwiML where

main = putStrLn "Hello World"

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

voiceResponse :: VoiceVerbs -> VoiceResponse
voiceResponse = undefined

{-
voiceResponse $ do
	say "Hello, world!" $ do
		voice ?= Man
		language ?= BritishEnglish
		loop ?= 1
	play "http://somemusic.com/music.mp3" (loop ?= 5)
	say' "Just the facts, ma'am."
	hangUp
-}

smsResponse :: SmsVerbs -> SmsResponse
smsResponse = undefined

{-
smsResponse $ do
	sms "Hello, world!"
	redirect "http://telephony.iankduncan.com"
-}


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

data Record = Record
	{ action :: Maybe Text
	, method :: Maybe Method
	, timeout :: Maybe Int
	, finishOnKey :: Maybe Key
	, maxLength :: Maybe Int
	, transcribe :: Maybe Bool
	, transcribeCallback :: Maybe Text
	, playBeep :: Maybe Bool
	}

data Sms = Sms
	{ to :: Text
	, from :: Text
	, action :: Maybe Text
	, method :: Maybe Method
	, statusCallback :: Maybe Text
	, text :: Text
	}

data TelephonyDevice
	= Number { sendDigits :: Maybe Text, url :: Maybe Text, number :: Text }
	| Sip { url :: Maybe Text, method :: Maybe Method, username :: Maybe Text, password :: Maybe Text, sipEndpoint :: Text }
	| Client { url :: Maybe Text, method :: Maybe Method, identifier :: Text }
	| Queue { url :: Maybe Text, method :: Maybe Method, identifier :: Text }
	| Conference
		{ muted :: Maybe Bool
		, beep :: Maybe Bool
		, startConferenceOnEnter :: Maybe Bool
		, endConferenceOnExit :: Maybe Bool
		, waitUrl :: Maybe Text
		, waitMethod :: Maybe Method
		, maxParticipants :: Maybe Int
		, conferenceRoom :: Text
		}

data Dial = Dial
	{ action :: Maybe Text
	, method :: Maybe Method
	, timeout :: Maybe Int
	, hangupOnStar :: Maybe Bool
	, timeLimit :: Maybe Int
	, callerId :: Maybe Text
	, telephonyDevices :: [TelephonyDevice]
	}

data Enqueue = Enqueue
	{ action :: Maybe Text
	, method :: Maybe Method
	, waitUrl :: Maybe Text
	, waitUrlMethod :: Maybe Method
	, identifier :: Text
	}

data Leave = Leave

data HangUp = HangUp

data Redirect = Redirect
	{ method :: Maybe Method
	, url :: Text
	}

data RejectionReason = Rejected | Busy

data Reject = Reject { reason :: Maybe RejectionReason }

data Pause = Pause
	{ _pauseLength :: Maybe Int
	}
