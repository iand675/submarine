module GitHub.Gists.Comments where
import GitHub.Internal

gistComments i = "/gists/" <> i <> "/comments"
gistComment gi ci = gistComments gi <> "/" <> ci
--| GET /gists/:gist_id/comments
listGistComments ::
	Int ->
	GitHub GistCommentsData
listGistComments = ghGet . gistComments

--| GET /gists/:gist_id/comments/:id
getGistComment ::
	Int ->
	Int ->
	GitHub GistCommentData
getGistComment gi ci = ghGet $ gistComment gi ci

--| POST /gists/:gist_id/comments
createGistComment ::
	Int ->
	NewGistComment ->
	GitHub GistCommentData
createGistComment i = ghPost (gistComments i)

--| PATCH /gists/:gist_id/comments/:id
editGistComment ::
	Int ->
	Int ->
	GistCommentPatch ->
	GitHub GistCommentData
editGistComment gi ci = ghPatch $ gistComment gi ci

--| DELETE /gists/:gist_id/comments/:id
deleteGistComment ::
	Int ->
	Int ->
	GitHub ()
deleteGistComment gi ci = ghDelete $ gistComment gi ci
