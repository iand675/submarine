module ElasticSearch.Main where

import ElasticSearch.Internal

data VersionInfo = VersionInfo
	{ number        :: Text
	, buildHash     :: Text
	, buildSnapshot :: Bool
	, luceneVersion :: Text
	}

data MainResponse = MainResponse
	{ ok      :: Bool
	, status  :: Int
	, name    :: Maybe Text
	, version :: VersionData
	, tagline :: Text
	}

get /
head /
