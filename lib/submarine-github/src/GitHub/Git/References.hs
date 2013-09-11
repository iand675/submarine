module GitHub.Git.References where
import GitHub.Internal

refs o r = ownerRepo o r <> "/git/refs"
ref o r r' = refs o r <> "/" <> r'

--| GET /repos/:owner/:repo/git/refs/:ref
getReference ::
	OwnerName ->
	RepoName ->
	Ref ->
	GitHub ReferenceData
getReference o r r' = get [] $ ref o r r'

--| GET /repos/:owner/:repo/git/refs
getAllReferences ::
	OwnerName ->
	RepoName ->
	GitHub ReferencesData
getAllReferences o r = get [] $ refs o r

--| POST /repos/:owner/:repo/git/refs
createReference ::
	OwnerName ->
	RepoName ->
	NewRef ->
	GitHub ReferenceData
createReference o r = post [] $ refs o r

--| PATCH /repos/:owner/:repo/git/refs/:ref
updateReference ::
	OwnerName ->
	RepoName ->
	RefPatch ->
	GitHub ReferenceData
updateReference o r r' = patch [] $ ref o r r'

--| DELETE /repos/:owner/:repo/git/refs/:ref
deleteReference ::
	OwnerName ->
	RepoName ->
	Ref ->
	GitHub ()
deleteReference o r r' = delete [] $ ref o r r'

