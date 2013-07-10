module GitHub.Users.Keys where

currentUserKeys = currentUserKeys <> "/keys"
currentUserKey i = currentUserKeys <> "/" <> i

--| GET /users/:user/keys
listUserKeys ::
	UserName ->
	GitHub PublicUserKeysData

--| GET /user/keys
listCurrentUserKeys ::
	GitHub UserKeysData
listCurrentUserKeys = ghGet [] currentUserKeys

--| GET /user/keys/:id
getKey ::
	Int ->
	GitHub UserKeyData
getKey = ghGet [] . currentUserKey

--| POST /user/keys
createKey ::
	NewKey ->
	GitHub UserKeyData
createKey = ghPost currentUserKeys

--| PATCH /user/keys
updateKey ::
	Int ->
	KeyPatch ->
	GitHub UserKeyData
updateKey i = ghPatch (currentUserKey i)

--| DELETE /user/keys/:id
deleteKey ::
	Int ->
	GitHub ()
deleteKey i = ghDelete . currentUserKey
