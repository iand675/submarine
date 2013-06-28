module Server.Tasks where

data Config = Config
	{ _configMandrillSettings
	, _configElasticSearchSettings
	, _configPostgresSettings
	, _configIntercomSettings
	--, _configNewRelicSettings
	, _configStripeSettings
	, _configTwilioSettings
	}

type App = ReaderT Config IO

elasticSearch :: ElasticSearch a -> App a
intercom :: Intercom a -> App a
mandrill :: Mandrill a -> App a
postgres :: Postgres a -> App a
stripe :: Stripe a -> App a
twilio :: Twilio a -> App a

