module Network.ElasticSearch where

-- CORE
-- index
indexDocument :: ToJSON a => Index -> Type -> Maybe Id -> IndexOptions -> a -> ElasticSearch IndexResponse
indexDocument = put [url| /{index}/{type}/{mId}{?indexOptions*} |]

-- delete
deleteDocument :: Index -> Type -> Id -> DeleteOptions -> ElasticSearch DeleteResponse

-- get
getDocument :: Index -> Maybe Type -> Id -> GetOptions -> ElasticSearch GetResponse

-- multi-get
data AllIndexItem = AllIndexItem { allIndexIndex :: Index, allIndexType :: Type, allIndexId :: Id }
data IndexedItem = IndexedItem { indexedType, indexedId }

data MultiGet 
  = FromAllIndices [AllIndexItem]
  | FromIndex [IndexItem]
  | FromIndexAndType [IndexTypeItem]

multiGetDocuments :: FromJSON a => MultiGet -> MultiGetOptions -> ElasticSearch (MultiGetResponse a)

-- update
updateDocument :: (ToJSON a, FromJSON b) => Index -> Type -> Id -> UpdateOptions a -> ElasticSearch (UpdateResponse b)

-- search
search :: (FromJSON a) => Index -> Type -> SearchOptions -> ElasticSearch (SearchResult a)

-- percolate
registerPercolator :: Index -> Name -> SearchQuery -> ElasticSearch PercolatorRegistrationResponse
percolate :: (FromJSON a) => Index -> Type -> PercolationOptions -> a -> ElasticSearch PercolationResponse

-- bulk
bulk :: [BulkAction] -> ElasticSearch BulkResponse

-- bulk udp

-- count
count :: [Index] -> [Type] -> CountOptions -> ElasticSearch CountResponse

-- delete by query
deleteByQuery :: [Index] -> [Type] -> DeleteByQueryOptions -> ElasticSearch DeleteByQueryResponse

-- more like this
moreLikeThis :: Index -> Type -> Id -> MoreLikeThisOptions -> ElasticSearch MoreLikeThisResponse

-- validate
validate :: Index -> Maybe Type -> Maybe Id -> SearchQuery
-- explain

-- INDICES
-- aliases
-- analyze
-- create index
-- delete index
-- open/close index
-- get settings
-- get mapping
-- put mapping
-- delete mapping
-- optimize
-- flush
-- snapshot
-- update settings
-- templates
-- warmers
-- stats
-- status
-- segments
-- clear cache
-- indices exists
-- types exists

-- CLUSTER
-- health
-- state
-- update settings
-- nodes info
-- nodes stats
-- nodes shutdown
-- nodes hot threads
-- cluster reroute
