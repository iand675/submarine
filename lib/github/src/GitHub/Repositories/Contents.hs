module GitHub.Repositories.Contents where
import GitHub.Internal

contents o r p = ownerRepo <> "/contents/" <> p
--| GET /repos/:owner/:repo/readme
getReadme ::
	OwnerName ->
	RepoName ->
	Maybe Ref ->
	GitHub FileData

--| GET /repos/:owner/:repo/contents/:path
getRepositoryPathContents ::
	OwnerName ->
	RepoName ->
	Path ->
	Maybe Text ->
	Maybe Ref ->
	GitHub FileData
getRepositoryPathContents o r p = get [] $ contents o r p

--| PUSH /repos/:owner/:repo/contents/:path
createFile ::
	OwnerName ->
	RepoName ->
	Path ->
	NewFile ->
	GitHub NewFileData
createFile o r p = get [] $ contents o r p

--| PUT /repos/:owner/:repo/contents/:path
updateFile ::
	OwnerName ->
	RepoName ->
	Path ->
	NewFile ->
	GitHub NewFileData
updateFile o r p = put [] $ contents o r p

--| DELETE /repos/:owner/:repo/contents/:path
deleteFile ::
	OwnerName ->
	RepoName ->
	Path ->
	DeleteFile ->
	GitHub NewFileData
deleteFile o r p = delete [] $ contents o r p

--| GET /repos/:owner/:repo/:archive_format/:ref
getArchive ::
	OwnerName ->
	RepoName ->
	ArchiveFormat ->
	Maybe Ref ->
	GitHub ByteString
--getArchive o r a ref =
