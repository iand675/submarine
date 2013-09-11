module Network.ElasticSearch where

data ElasticSearchM a

clusterHealth :: ElasticSearchM a
clusterHealth = undefined
nodesHotThreads :: ElasticSearchM a
nodesHotThreads = undefined
nodesInfo :: ElasticSearchM a
nodesInfo = undefined
nodesRestart :: ElasticSearchM a
nodesRestart = undefined
nodesShutdown :: ElasticSearchM a
nodesShutdown = undefined
nodesStats :: ElasticSearchM a
nodesStats = undefined
clusterReroute :: ElasticSearchM a
clusterReroute = undefined
clusterUpdateSettings :: ElasticSearchM a
clusterUpdateSettings = undefined
clusterSearchShards :: ElasticSearchM a
clusterSearchShards = undefined
clusterState :: ElasticSearchM a
clusterState = undefined
pendingClusterTasks :: ElasticSearchM a
pendingClusterTasks = undefined
indicesGetAliases :: ElasticSearchM a
indicesGetAliases = undefined
indicesAliases :: ElasticSearchM a
indicesAliases = undefined
analyze :: ElasticSearchM a
analyze = undefined
clearIndicesCache :: ElasticSearchM a
clearIndicesCache = undefined
closeIndex :: ElasticSearchM a
closeIndex = undefined
createIndex :: ElasticSearchM a
createIndex = undefined
deleteIndex :: ElasticSearchM a
deleteIndex = undefined
indicesExists :: ElasticSearchM a
indicesExists = undefined
typesExists :: ElasticSearchM a
typesExists = undefined
flush :: ElasticSearchM a
flush = undefined
gatewaySnapshot :: ElasticSearchM a
gatewaySnapshot = undefined
deleteMapping :: ElasticSearchM a
deleteMapping = undefined
putMapping :: ElasticSearchM a
putMapping = undefined
openIndex :: ElasticSearchM a
openIndex = undefined
optimize :: ElasticSearchM a
optimize = undefined
refresh :: ElasticSearchM a
refresh = undefined
indicesSegments :: ElasticSearchM a
indicesSegments = undefined
updateSettings :: ElasticSearchM a
updateSettings = undefined
indicesStats :: ElasticSearchM a
indicesStats = undefined
indicesStatus :: ElasticSearchM a
indicesStatus = undefined
deleteIndexTemplate :: ElasticSearchM a
deleteIndexTemplate = undefined
getIndexTemplates :: ElasticSearchM a
getIndexTemplates = undefined
putIndexTemplate :: ElasticSearchM a
putIndexTemplate = undefined
validateQuery :: ElasticSearchM a
validateQuery = undefined
deleteWarmer :: ElasticSearchM a
deleteWarmer = undefined
putWarmer :: ElasticSearchM a
putWarmer = undefined
bulk :: ElasticSearchM a
bulk = undefined
count :: ElasticSearchM a
count = undefined
delete :: ElasticSearchM a
delete = undefined
indexDelete :: ElasticSearchM a
indexDelete = undefined
deleteByQuery :: ElasticSearchM a
deleteByQuery = undefined
indexDeleteByQuery :: ElasticSearchM a
indexDeleteByQuery = undefined
explain :: ElasticSearchM a
explain = undefined
get :: ElasticSearchM a
get = undefined
multiGet :: ElasticSearchM a
multiGet = undefined
index :: ElasticSearchM a
index = undefined
moreLikeThis :: ElasticSearchM a
moreLikeThis = undefined
percolate :: ElasticSearchM a
percolate = undefined
multiSearch :: ElasticSearchM a
multiSearch = undefined
search :: ElasticSearchM a
search = undefined
searchScroll :: ElasticSearchM a
searchScroll = undefined
suggest :: ElasticSearchM a
suggest = undefined
update :: ElasticSearchM a
update = undefined