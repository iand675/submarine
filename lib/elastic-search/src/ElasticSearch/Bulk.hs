module ElasticSearch.Bulk where

data BulkResponse = BulkResponse
	{ took  :: Int
	, items :: Vector BulkResponseItem
	}

data BulkResponseItem = BulkResponseItem
	{ _index
	, _type
	, _id
	, error
	, ok
	, _version
	, matches
	, found
	}

post /_bulk
put  /_bulk
post /{index}/_bulk
put  /{index}/_bulk
post /{index}/{type}/_bulk
put  /{index}/{type}/_bulk