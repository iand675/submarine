data Priority = Urgent | High | Normal | Low | Languid
data NodeRange = Exactly Int | LessThan Int | GreaterThan Int
data ClusterHealthStatus = Green | Yellow | Red
data IgnoreIndices = Default | None | Missing
data BulkRequestItem = Index IndexRequest | Delete DeleteRequest
data VersionType = Internal | External
data IndexOperation = Index | Create

[declare|
ClusterShardHealth
	activeShards :: Int
	relocatingShards :: Int
	initializingShards :: Int
	unassignedShards :: Int
	primaryActive :: Bool
	status :: ClusterHealthStatus
	shardId :: Int

ClusterIndexHealth
	index :: Text
	validationFailures :: Vector Text
	numberOfShards :: Int
	numberOfReplicas :: Int
	activeShards :: Int
	relocatingShards :: Int
	activePrimaryShards :: Int
	initializingShards :: Int
	unassignedShards :: Int
	status :: ClusterHealthStatus
	shards :: HashMap Int ClusterShardHealth

ClusterHealthRequest < MasterNodeOperationRequest
	indices :: Vector Index
	timeout :: TimeValue
	waitForHealthStatus :: Maybe ClusterHealthStatus
	waitForRelocatingShards :: Int
	waitForActiveShards :: Int
	waitForNodes :: Maybe NodeRange
	local :: Bool
	waitForEvents :: Maybe Priority

ClusterHealthResponse
	clusterName :: Text
	numberOfNodes :: Int
	numberOfDataNodes :: Int
	activeShards :: Int
	relocatingShards :: Int
	activePrimaryShards :: Int
	initializingShards :: Int
	timedOut :: Bool
	status :: ClusterHealthStatus
	validationFailures :: Vector Text
	indices :: HashMap Text ClusterIndexHealth

NodesHotThreadsRequest < NodesOperationRequest
	threads :: Int
	type :: Text
	interval :: TimeValue
	snapshots :: Int

NodesHotThreadsResponse < NodesOperationResponse NodeHotThreads

NodesInfoRequest < NodesOperationRequest
	settings :: Bool
	os :: Bool
	process :: Bool
	jvm :: Bool
	threadPool :: Bool
	network :: Bool
	transport :: Bool
	http :: Bool
	plugin :: Bool

NodesInfoResponse < NodesOperationResponse NodeInfo
	settingsFilter :: SettingsFilter

NodesRestartRequest < NodesOperationRequest
	delay :: TimeValue

NodeRestartResponse < NodeOperationResponse
	node :: DiscoveryNode

NodesRestartResponse < NodesOperationResponse NodeRestartResponse

NodesShutdownRequest < MasterNodeOperationRequest
	nodeIds :: Vector Text
	delay :: TimeValue
	exit :: Bool

NodesShutdownResponse
	clusterName :: ClusterName
	nodes :: Vector DiscoveryNode

NodesStatsRequest < NodesOperationRequest
	indices :: CommonStatsFlags
	os :: Bool
	process :: Bool
	jvm :: Bool
	threadPool :: Bool
	network :: Bool
	fs :: Bool
	transport :: Bool
	http :: Bool

NodesStatsResponse < NodesOperationResponse NodeStats

ClusterRerouteRequest < MasterNodeOperation
	commands :: AllocationCommands
	dryRun :: Bool

ClusterRerouteResponse
	state :: ClusterState

ClusterUpdateSettingsRequest < MasterNodeOperationRequest
	transientSettings :: Settings
	persistentSettings :: Settings

ClusterUpdateSettingsResponse
	transientSettings :: Settings
	persistentSettings :: Settings

ClusterSearchShardsRequest < MasterNodeOperationRequest
	indices :: Vector Text
	routing :: Maybe Text
	preference :: Maybe Text
	local :: Bool
	types :: Vector Text
	ignoreIndices :: IgnoreIndices

ClusterSearchShardsResponse
	groups :: Vector ClusterSearchShardsGroup
	nodes :: Vector DiscoveryNode

ClusterStateRequest < MasterNodeOperationRequest
	filterRoutingTable :: Bool
	filterNodes :: Bool
	filterMetadata :: Bool
	filterBlocks :: Bool
	filteredIndices :: Vector Text
	filteredIndexTemplates :: Vector Text
	local :: Bool

ClusterStateResponse
	clusterName :: ClusterName
	clusterState :: ClusterState

PendingClusterTasksRequest < MasterNodeOperationRequest

PendingClusterTasksResponse
	pendingTasks :: Vector PendingClusterTask

AliasesExistResponse
	exists :: Bool

IndicesGetAliasesRequest < MasterNodeOperationRequest
	indices :: Vector Text
	aliases :: Vector Text
	ignoreIndices :: IgnoreIndices

IndicesGetAliasesResponse
	aliases :: HashMap Text (Vector AliasMetadata)

IndicesAliasesRequest < MasterNodeOperationRequest
	aliasActions :: Vector AliasAction
	timeout :: TimeValue

IndicesAliasesResponse
	acknowledged :: Bool

AnalyzeRequest < SingleCustomOperationRequest
	index :: Text
	text :: Text
	analyzer :: Text
	tokenizer :: Text
	tokenFilters :: Vector Text
	field :: Text

AnalyzeToken
	term :: Text
	startOffset :: Int
	endOffset :: Int
	position :: Int
	type :: Text

AnalyzeResponse
	tokens :: Vector AnalyzeToken

ClearIndicesCacheRequest < BroadcastOperationRequest
	filterCache :: Bool
	fieldDataCache :: Bool
	idCache :: Bool
	recycler :: Bool
	fields :: Vector Text
	filterKeys :: Vector Text

ClearIndicesCacheResponse < BroadcastOperationResponse

CloseIndexRequest < MasterNodeOperationRequest
	index :: Text
	timeout :: TimeValue

CloseIndexResponse
	acknowledged :: Bool

CreateIndexRequest < MasterNodeOperationRequest
	cause :: Text
	index :: Text
	settings :: Settings
	mappings :: HashMap Text Text
	customs :: HashMap Text IndexMetadata.Custom
	timeout :: TimeValue

CreateIndexResponse
	acknowledged :: Bool

DeleteIndexRequest < MasterNodeOperationRequest
	indices :: Vector Text
	timeout :: TimeValue

DeleteIndexResponse
	acknowledged :: Bool

IndicesExistsRequest < MasterNodeOperationRequest
	indices :: Vector Text

IndicesExistsResponse
	exists :: Bool

TypesExistsRequest < MasterNodeOperationRequest
	indices :: Vector Text
	types :: Vector Text
	ignoreIndices :: IgnoreIndices

TypesExistsResponse
	exists :: Bool

FlushRequest < BroadcastOperationRequest
	refresh :: Bool
	force :: Bool
	full :: Bool

FlushResponse < BroadcastOperationResponse

GatewaySnapshotRequest < BroadcastOperationRequest

GatewaySnapshotResponse < BroadcastOperationResponse

DeleteMappingRequest < MasterNodeOperationRequest
	indices :: Vector Text
	type :: Text

DeleteMappingResponse

PutMappingRequest < MasterNodeOperationRequest
	indices :: Vector Text
	type :: Text
	source :: Text
	timeout :: TimeValue
	ignoreConflicts :: Bool

PutMappingResponse
	acknowledged :: Bool

OpenIndexRequest < MasterNodeOperationRequest
	index :: Text
	timeout :: TimeValue

OpenIndexResponse
	acknowledged :: Bool

OptimizeRequest < BroadcastOperationRequest
	waitForMerge :: Bool
	maxNumSegments :: Int
	onlyExpungeDeletes :: Bool
	flush :: Bool
	refresh :: Bool

OptimizeResponse < BroadcastOperationResponse

RefreshRequest < BroadcastOperationRequest
	force :: Bool

RefreshResponse < BroadcastOperationResponse

IndicesSegmentsRequest < BroadcastOperationRequest

IndicesSegmentsResponse < BroadcastOperationResponse
	shards :: Vector ShardSegments
	indicesSegments :: HashMap Text IndexSegments

UpdateSettingsRequest < MasterNodeOperationRequest
	indices :: Vector Text
	settings :: Settings

UpdateSettingsResponse

IndicesStatsRequest < BroadcastOperationRequest
	flags :: CommonStatsFlags

IndicesStatsResponse < BroadcastOperationResponse
	shards :: Vector ShardStats

IndicesStatusRequest < BroadcastOperationRequest
	recovery :: Bool
	snapshot :: Bool

IndicesStatusResponse < BroadcastOperationResponse
	shards :: Vector ShardStatus
	indicesStatus :: HashMap Text IndexStatus

DeleteIndexTemplateRequest < MasterNodeOperationRequest
	name :: Text
	timeout :: TimeValue

DeleteIndexTemplateResponse
	acknowledged :: Bool

GetIndexTemplatesRequest < MasterNodeOperationRequest
	name :: Text

GetIndexTemplatesResponse
	indexTemplates :: Vector IndexTemplateMetaData

PutIndexTemplateRequest < MasterNodeOperationRequest
	name :: Text
	cause :: Text
	template :: Text
	order :: Int
	create :: Bool
	settings :: Settings
	mappings :: HashMap Text Text
	customs :: HashMap Text Custom
	timeout :: TimeValue

PutIndexTemplateResponse
	acknowledged :: Bool

ValidateQueryRequest < BroadcastOperationRequest
	querySource :: Builder
	querySourceUnsafe :: Bool
	explain :: Bool
	types :: Vector Text

ValidateQueryResponse < BroadcastOperationResponse
	isValid :: Bool
	queryExplanations :: Vector QueryExplanation

DeleteWarmerRequest < MasterNodeOperationRequest
	name :: Text
	indices :: Vector Text

DeleteWarmerResponse
	acknowledged :: Bool

PutWarmerRequest < MasterNodeOperationRequest
	name :: Text
	searchRequest :: SearchRequest

BulkRequest
	requests :: Vector BulkRequestItem
	payloads :: Vector Object
	replicationType :: ReplicationType
	consistencyLevel :: WriteConsistencyLevel
	refresh :: Bool

BulkResponse
	responses :: Vector BulkItemResponse
	tookInMillis :: Long

CountRequest < BroadcastOperationRequest
	minScore :: Float
	routing :: Maybe Text
	preference :: Maybe Text
	querySource :: Builder
	querySourceUnsafe :: Bool
	types :: Vector Text

CountResponse < BroadcastOperationResponse
	count :: Long

DeleteRequest < ShardReplicationOperationRequest
	type :: Text
	id :: Text
	routing :: Maybe Text
	refresh :: Bool
	version :: Long
	versionType :: VersionType

DeleteResponse
	index :: Text
	id :: Text
	type :: Text
	version :: Long
	notFound :: Bool

IndexDeleteRequest < IndexReplicationOperationRequest
	type :: Text
	id :: Text
	refresh :: Bool
	version :: Long

IndexDeleteResponse
	index :: Text
	successfulShards :: Int
	failedShards :: Int
	deleteResponses :: Vector ShardDeleteResponse

DeleteByQueryRequest < IndicesReplicationOperationRequest
	querySource :: Builder
	querySourceUnsafe :: Bool
	types :: Vector Text
	routing :: Maybe Text

DeleteByQueryResponse
	indices :: HashMap Text IndexDeleteByQueryResponse

IndexDeleteByQueryRequest < IndexReplicationOperationRequest
	querySource :: Builder
	types :: Vector Text
	routing :: Maybe (Set Text)
	filteringAliases :: Maybe (Vector Text)

IndexDeleteByQueryResponse
	index :: Text
	successfulShards :: Int
	failedShards :: Int

ExplainRequest < SingleShardOperationRequest
	type :: Text
	id :: Text
	routing :: Text
	preference :: Text
	source :: Builder
	sourceUnsafe :: Bool
	fields :: Vector Text

ExplainResponse
	exists :: Bool
	explanation :: Explanation
	getResult :: GetResult

GetRequest < SingleShardOperationRequest
	type :: Text
	id :: Text
	routing :: Text
	preference :: Text
	fields :: Vector Text
	refresh :: Bool
	realtime :: Bool

GetResponse
	getResult :: GetResult

MultiGetRequest
	preference :: Text
	realtime :: Bool
	refresh :: Bool
	items :: Vector Item

MultiGetResponse
	responses :: Vector MultiGetItemResponse

IndexRequest < ShardReplicationOperationRequest
	type :: Text
	id :: Text
	routing :: Maybe Text
	parent :: Maybe Text
	timestamp :: Maybe Text
	ttl :: Maybe Long
	source :: Builder
	sourceUnsafe :: Bool
	opType :: IndexOperation
	refresh :: Bool
	version :: Long
	versionType :: VersionType
	percolate :: Text
	contentType :: XContentType

IndexResponse
	index :: Text
	id :: Text
	type :: Text
	version :: Long
	matches :: Vector Text

MoreLikeThisRequest
	index :: Text
	type :: Text
	id :: Text
	routing :: Text
	fields :: Vector Text
	percentTermsToMatch :: Maybe Float
	minTermFreq :: Maybe Int
	maxQueryTerms :: Maybe Int
	stopWords :: Maybe (Vector Text)
	minDocFreq :: Maybe Int
	maxDocFreq :: Maybe Int
	minWordLen :: Maybe Int
	maxWordLen :: Maybe Int
	boostTrems :: Maybe Float
	searchType :: SearchType
	searchSize :: Int
	searchFrom :: Int
	searchQueryHint :: Text
	searchIndices :: Vector Text
	searchTypes :: Vector Text
	searchScroll :: Scroll
	searchSource :: Builder
	searchSourceUnsafe :: Bool

PercolateRequest < SingleCustomOperationRequest
	index :: Text
	type :: Text
	source :: Builder
	sourceUnsafe :: Bool

MultiSearchRequest
	requests :: Vector SearchRequest
	ignoreIndices :: IgnoreIndices

MultiSearchResponse
	items :: Vector SearchResponseItem

SearchRequest
	searchType :: SearchType
	indices :: Vector Text
	routing :: Maybe Text
	preference :: Maybe Text
	source :: Builder
	sourceUnsafe :: Bool
	extraSource :: Builder
	extraSourceUnsafe :: Bool
	scroll :: Scroll
	types :: Vector Text
	operationThreading :: SearchOperationThreading
	ignoreIndices :: IgnoreIndices

SearchResponse
	scrollId :: Text
	totalShards :: Int
	successfulShards :: Int
	shardFailures :: Vector ShardSearchFailure
	tookInMillis :: Long
	hits :: SearchHits
	facets :: Facets
	suggest :: Suggest
	isTimedOut :: Bool

SearchScrollRequest
	scrollId :: Text
	scroll :: Scroll
	operationThreading :: SearchOperationThreading

SuggestRequest < BroadcastOperationRequest
	routing :: Maybe Text
	preference :: Maybe Text
	suggestSource :: Builder
	suggestSourceUnsafe :: Bool

SuggestResponse < BroadcastOperationResponse
	suggest :: Suggest

UpdateRequest < InstanceShardOperationRequest
	type :: Text
	id :: Text
	routing :: Maybe Text
	script :: Maybe Text
	scriptLang :: Maybe Text
	scriptParams :: HashMap Text Object
	fields :: Vector Text
	retryOnConflict :: Int
	percolate :: Text
	refresh :: Bool
	replicationType :: ReplicationType
	consistencyLevel :: WriteConsistencyLevel
	upsertRequest :: IndexRequest
	docAsUpsert :: Bool
	doc :: Maybe IndexRequest

UpdateResponse
	index :: Text
	id :: Text
	type :: Text
	version :: Long
	matches :: Vector Text
	getResult :: GetResult
|]

data ElasticSearch a where
	ClusterHealth :: ClusterHealthRequest -> ElasticSearch ClusterHealthResponse
	NodesHotThreads :: NodesHotThreadsRequest -> ElasticSearch NodesHotThreadsResponse
	NodesInfo :: NodesInfoRequest -> ElasticSearch NodesInfoResponse
	NodesRestart :: NodesRestartRequest -> ElasticSearch NodesRestartResponse
	NodesShutdown :: NodesShutdownRequest -> ElasticSearch NodesShutdownResponse
	NodesStats :: NodesStatsRequest -> ElasticSearch NodesStatsResponse
	ClusterReroute :: ClusterRerouteRequest -> ElasticSearch ClusterRerouteResponse
	ClusterUpdateSettings :: ClusterUpdateSettingsRequest -> ElasticSearch ClusterUpdateSettingsResponse
	ClusterSearchShards :: ClusterSearchShardsRequest -> ElasticSearch ClusterSearchShardsResponse
	ClusterState :: ClusterStateRequest -> ElasticSearch ClusterStateResponse
	PendingClusterTasks :: PendingClusterTasksRequest -> ElasticSearch PendingClusterTasksResponse
	IndicesGetAliases :: IndicesGetAliasesRequest -> ElasticSearch IndicesGetAliasesResponse
	IndicesAliases :: IndicesAliasesRequest -> ElasticSearch IndicesAliasesResponse
	Analyze :: AnalyzeRequest -> ElasticSearch AnalyzeResponse
	ClearIndicesCache :: ClearIndicesCacheRequest -> ElasticSearch ClearIndicesCacheResponse
	CloseIndex :: CloseIndexRequest -> ElasticSearch CloseIndexResponse
	CreateIndex :: CreateIndexRequest -> ElasticSearch CreateIndexResponse
	DeleteIndex :: DeleteIndexRequest -> ElasticSearch DeleteIndexResponse
	IndicesExists :: IndicesExistsRequest -> ElasticSearch IndicesExistsResponse
	TypesExists :: TypesExistsRequest -> ElasticSearch TypesExistsResponse
	Flush :: FlushRequest -> ElasticSearch FlushResponse
	GatewaySnapshot :: GatewaySnapshotRequest -> ElasticSearch GatewaySnapshotResponse
	DeleteMapping :: DeleteMappingRequest -> ElasticSearch DeleteMappingResponse
	PutMapping :: PutMappingRequest -> ElasticSearch PutMappingResponse
	OpenIndex :: OpenIndexRequest -> ElasticSearch OpenIndexResponse
	Optimize :: OptimizeRequest -> ElasticSearch OptimizeResponse
	Refresh :: RefreshRequest -> ElasticSearch RefreshResponse
	IndicesSegments :: IndicesSegmentsRequest -> ElasticSearch IndicesSegmentsResponse
	UpdateSettings :: UpdateSettingsRequest -> ElasticSearch UpdateSettingsResponse
	IndicesStats :: IndicesStatsRequest -> ElasticSearch IndicesStatsResponse
	IndicesStatus :: IndicesStatusRequest -> ElasticSearch IndicesStatusResponse
	DeleteIndexTemplate :: DeleteIndexTemplateRequest -> ElasticSearch DeleteIndexTemplateResponse
	GetIndexTemplates :: GetIndexTemplatesRequest -> ElasticSearch GetIndexTemplatesResponse
	PutIndexTemplate :: PutIndexTemplateRequest -> ElasticSearch PutIndexTemplateResponse
	ValidateQuery :: ValidateQueryRequest -> ElasticSearch ValidateQueryResponse
	DeleteWarmer :: DeleteWarmerRequest -> ElasticSearch DeleteWarmerResponse
	PutWarmer :: PutWarmerRequest -> ElasticSearch PutWarmerResponse
	Bulk :: BulkRequest -> ElasticSearch BulkResponse
	Count :: CountRequest -> ElasticSearch CountResponse
	Delete :: DeleteRequest -> ElasticSearch DeleteResponse
	IndexDelete :: IndexDeleteRequest -> ElasticSearch IndexDeleteResponse
	DeleteByQuery :: DeleteByQueryRequest -> ElasticSearch DeleteByQueryResponse
	IndexDeleteByQuery :: IndexDeleteByQueryRequest -> ElasticSearch IndexDeleteByQueryResponse
	Explain :: ExplainRequest -> ElasticSearch ExplainResponse
	Get :: GetRequest -> ElasticSearch GetResponse
	MultiGet :: MultiGetRequest -> ElasticSearch MultiGetResponse
	Index :: IndexRequest -> ElasticSearch IndexResponse
	MoreLikeThis :: MoreLikeThisRequest -> ElasticSearch SearchResponse
	Percolate :: PercolateRequest -> ElasticSearch PercolateResponse
	MultiSearch :: MultiSearchRequest -> ElasticSearch MultiSearchResponse
	Search :: SearchRequest -> ElasticSearch SearchResponse
	SearchScroll :: SearchScrollRequest -> ElasticSearch SearchResponse
	Suggest :: SuggestRequest -> ElasticSearch SuggestResponse
	Update :: UpdateRequest -> ElasticSearch UpdateResponse
