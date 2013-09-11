module GitHub.Repositories.Keys where
import GitHub.Internal

repoKeys o r = ownerRepo o r <> "/keys"
repoKey o r i = repoKeys o r <> "/" <> i

--| GET /repos/:owner/:repo/keys
listKeys ::
	OwnerName ->
	RepoName ->
	GitHub GroupKeysData
listKeys o r = get [] $ repoKeys o r

--| GET /repos/:owner/:repo/keys/:id
getKey ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub GroupKeyData
getKey o r i = get [] $ repoKey o r i

--| POST /repos/:owner/:repo/keys
createKey ::
	OwnerName ->
	RepoName ->
	NewGroupKey ->
	GitHub GroupKeyData
createKey o r = post [] (repoKeys o r)

--| PATCH /repos/:owner/:repo/keys/:id
editKey ::
	OwnerName ->
	RepoName ->
	Int ->
	GroupKeyPatch ->
	GitHub GroupKeyData
editKey o r i = patch [] (repoKey o r i)

--| DELETE /repos/:owner/:repo/keys/:id
deleteKey ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub ()
deleteKey o r i = delete [] (repoKey o r i)
