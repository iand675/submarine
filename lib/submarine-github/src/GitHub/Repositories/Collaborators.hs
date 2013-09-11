module GitHub.Repositories.Collaborators where
import GitHub.Internal

collaborators o r = ownedRepo o r <> "/collaborators"
collaborator o r u = collaborators o r <> "/" <> u

--| GET /repos/:owner/:repo/collaborators
listCollaborators ::
	OwnerName ->
	RepoName ->
	GitHub CollaboratorsData
listCollaborators o r = get [] $ collaborators o r

--| GET /repos/:owner/:repo/collaborators/:user
getCollaborator ::
	OwnerName ->
	RepoName ->
	UserName ->
	GitHub CollaboratorData
getCollaborator o r u = get [] $ collaborator o r u

--| PUT /repos/:owner/:repo/collaborators/:user
addCollaborator ::
	OwnerName ->
	RepoName ->
	UserName ->
	GitHub ()
addCollaborator o r u = put [] $ collaborator o r u

--| DELETE /repos/:owner/:repo/collaborators/:user
removeCollaborator ::
	OwnerName ->
	RepoName ->
	UserName ->
	GitHub ()
removeCollaborator o r u = delete [] $ collaborator o r u
