module GitHub.Repositories.Comments where
import GitHub.Internal

shaComment s = ["/commits/", s, "/comments"]
commentId i = ["/comments", i]

--| GET /repos/:owner/:repo/comments
listRepositoryComments ::
	OwnerName ->
	RepoName ->
	GitHub CommentsData
listRepositoryComments o r = get [] (ownedRepo o r <> "/comments")

--| GET /repos/:owner/:repo/commits/:sha/comments
listCommitComments ::
	OwnerName ->
	RepoName ->
	Sha ->
	GitHub CommentsData
listCommitComments o r s = get [] (ownedRepo o r <> shaComment s)

--| POST /repos/:owner/:repo/commits/:sha/comments
createCommitComment ::
	OwnerName ->
	RepoName ->
	Sha ->
	NewCommitComment ->
	GitHub CommentData
createCommitComment o r s = post [] (ownedRepo o r <> shaComment s)

--| GET /repos/:owner/:repo/comments/:id
getCommitComment ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub CommentData
getCommitComment o r i = get [] (ownedRepo o r <> commentId i)

--| PATCH /repos/:owner/:repo/comments/:id
updateCommitComment ::
	OwnerName ->
	RepoName ->
	Int ->
	CommentPatch ->
	GitHub CommentData
updateCommitComment o r i = patch [] (ownedRepo o r <> commentId i)

--| DELETE /repos/:owner/:repo/comments/:id
deleteCommitComment ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub ()
deleteCommitComment o r i = delete [] (ownedRepo o r <> commentId i)
