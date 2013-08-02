module GitHub.Gists where
import GitHub.Internal

gists = "/gists"
public = gists <> "/public"
starred = gists <> "/starred"
gist i = gists <> "/" <> i
star i = gists i <> "/star"
forks i = gists i <> "/forks"

--| GET /users/:user/gists
listUserGists ::
	UserName ->
	GitHub GistsData
listUserGists u = ghGet (user u <> gists)

--| GET /gists
listCurrentUserGists ::
	GitHub GistsData
listCurrentUserGists = ghGet gists

--| GET /gists/public
listCurrentUserPublicGists ::
	GitHub GistsData
listCurrentUserPublicGists = ghGet public

--| GET /gists/starred
listCurrentUserStarredGists ::
	GitHub GistsData
listCurrentUserStarredGists = ghGet starred

--| GET /gists/:id
getGist ::
	Int ->
	GitHub GistData
getGist = ghGet . gist

--| POST /gists
createGist ::
	NewGist ->
	GitHub GistData
createGist = ghPost gists

--| PATCH /gists/:id
editGist ::
	Int ->
	GistPatch ->
	GitHub GistData
editGist i = ghPatch (gist i)

--| PUT /gists/:id/star
starGist ::
	Int ->
	GitHub ()
starGist = ghPut' . star

--| DELETE /gists/:id/star
unstarGist ::
	Int ->
	GitHub ()
unstarGist = ghDelete . star

--| GET /gists/:id/star
isGistStarred ::
	Int ->
	GitHub Bool
isGistStarred = ghGet . star

--| POST /gists/:id/forks
forkGist ::
	Int ->
	GitHub GistData
forkGist = ghPost' . forks

--| DELETE /gists/:id
deleteGist ::
	Int ->
	GitHub ()
deleteGist = ghDelete . gist
