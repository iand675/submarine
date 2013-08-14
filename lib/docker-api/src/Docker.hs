module Docker where

data ListContainerOptions
  = ListAll
  | Limit Int
  | Since ContainerId
  | Before ContainerId
  | ShowSize Bool

listContainers :: ListContainerOptions -> Docker [ContainerSummary]

createContainer :: NewContainer -> Docker CreatedContainerResponse
inspectContainer :: ContainerId -> Docker ContainerInfo
listRunningContainerProcesses :: ContainerId -> [PsArg] -> Docker RunningProcesses
inspectFilesystemChanges :: ContainerId -> Docker [FileSystemChange]
exportContainer :: ContainerId -> Docker OutputStream
startContainer :: ContainerId -> Maybe HostConfig -> Docker StartedContainerInfo
stopContainer :: ContainerId -> Docker ()
restartContainer :: ContainerId -> Timeout -> Docker ()
killContainer :: ContainerId -> Docker ()
attachContainer :: ContainerId -> AttachOptions -> Docker OutputStream
awaitContainerExit :: ContainerId -> Docker StatusCodeResult
removeContainer :: ContainerId -> RemoveOptions -> Docker ()
copyFile :: ContainerId -> CopyFileOptions -> Docker OutputStream
listImages :: ListFormat a -> Docker a
createImage :: CreationOptions -> Docker CreationStatus
insertFile :: ImageName -> InsertOptions -> Docker InsertionStatus
inspectImage :: ImageName -> Docker ImageInfo
getImageHistory :: ImageName -> Docker [HistoryInfo]
pushImage :: ImageName -> PushOptions -> Docker ()
tagImage :: ImageName -> TagOptions -> Docker ()
removeImage :: ImageName -> Docker [DeletionInfo]
searchImages :: SearchOptions -> Docker [SearchResult]
buildImage :: InputStream -> BuildOptions -> Docker OutputStream
checkAuthConfiguration :: Docker AuthInfo
getSystemInformation :: Docker SystemInfo
getDockerVersionInformation :: Docker VersionInfo
commitImageChanges :: CommitOptions -> Docker CommittedImage
