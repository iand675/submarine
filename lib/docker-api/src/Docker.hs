module Docker where

data ListContainerOptions
  = ListAll
  | Limit Int
  | Since ContainerId
  | Before ContainerId
  | ShowSize Bool

listContainers :: ListContainerOptions -> Docker [ContainerSummary]
listContainers 

createContainer :: NewContainer -> Docker CreatedContainerResponse
createContainer 

inspectContainer :: ContainerId -> Docker ContainerInfo
inspectContainer 

listRunningContainerProcesses :: ContainerId -> [PsArg] -> Docker RunningProcesses
listRunningContainerProcesses 

inspectFilesystemChanges :: ContainerId -> Docker [FileSystemChange]
inspectFilesystemChanges 

exportContainer :: ContainerId -> Docker OutputStream
exportContainer 

startContainer :: ContainerId -> Maybe HostConfig -> Docker StartedContainerInfo
startContainer 

stopContainer :: ContainerId -> Docker ()
stopContainer 

restartContainer :: ContainerId -> Timeout -> Docker ()
restartContainer 

killContainer :: ContainerId -> Docker ()
killContainer 

attachContainer :: ContainerId -> AttachOptions -> Docker OutputStream
attachContainer 

awaitContainerExit :: ContainerId -> Docker StatusCodeResult
awaitContainerExit 

removeContainer :: ContainerId -> RemoveOptions -> Docker ()
removeContainer 

copyFile :: ContainerId -> CopyFileOptions -> Docker OutputStream
copyFile 

listImages :: ListFormat a -> Docker a
listImages 

createImage :: CreationOptions -> Docker CreationStatus
createImage 

insertFile :: ImageName -> InsertOptions -> Docker InsertionStatus
insertFile 

inspectImage :: ImageName -> Docker ImageInfo
inspectImage 

getImageHistory :: ImageName -> Docker [HistoryInfo]
getImageHistory 

pushImage :: ImageName -> PushOptions -> Docker ()
pushImage 

tagImage :: ImageName -> TagOptions -> Docker ()
tagImage 

removeImage :: ImageName -> Docker [DeletionInfo]
removeImage 

searchImages :: SearchOptions -> Docker [SearchResult]
searchImages 

buildImage :: InputStream -> BuildOptions -> Docker OutputStream
buildImage 

checkAuthConfiguration :: Docker AuthInfo
checkAuthConfiguration 

getSystemInformation :: Docker SystemInfo
getSystemInformation 

getDockerVersionInformation :: Docker VersionInfo
getDockerVersionInformation 

commitImageChanges :: CommitOptions -> Docker CommittedImage
commitImageChanges 
