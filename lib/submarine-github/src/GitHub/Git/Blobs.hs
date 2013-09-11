module GitHub.Git.Blobs where
import GitHub.Internal

--| GET /repos/:owner/:repo/git/blobs/:sha
getBlob ::
	OwnerName ->
	RepoName ->
	Sha ->
	GitHub BlobData
getBlob o r s = get [] (ownerRepo o r <> "/git/blobs/" <> s)

--| POST /repos/:owner/:repo/git/blobs
createBlob ::
	OwnerName ->
	RepoName ->
	NewBlob ->
	GitHub BlobData
createBlob o r = post [] (ownerRepo o r <> "/git/blobs")
