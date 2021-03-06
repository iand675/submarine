module Twilio where
import API
import URI.TH

newtype Twilio a = Twilio { fromTwilio :: APIClient a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- TODO get easy-api to append API routes instead of overwriting the base path portion.
{-runTwilio :: AccountSid -> AuthToken -> Twilio a -> IO (Either APIError a)-}
{-runTwilio sid authToken = runAPIClient "https://api.twilio.com/2010-04-01" (applyBasicAuth sid authToken) . fromTwilio-}


-- REST API
{-
make a call
POST /accounts/{accountSid}/calls
	in:
		required
			From:
			To:
			either (Url or ApplicationSid)
		optional
			method
			fallbackUrl
			fallbackMethod
			statusCallback
			statusCallbackMethod
			sendDigits
			ifMachine
			timeout
			record

-}



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

