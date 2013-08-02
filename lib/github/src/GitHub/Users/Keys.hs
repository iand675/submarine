module GitHub.Users.Keys where
import GitHub.Internal

userKeys u = [uri| /users/{u}/keys |]
currentUserKeys = [uri| /users/keys |]
currentUserKey i = [uri| /users/keys/{i} |]

-- | GET /users/:user/keys
listUserKeys ::
	UserName ->
	GitHub PublicUserKeysData
listUserKeys = get . userKeys

-- | GET /user/keys
listCurrentUserKeys ::
	GitHub UserKeysData
listCurrentUserKeys = get "/user/keys"

-- | GET /user/keys/:id
getKey ::
	Int ->
	GitHub UserKeyData
getKey = get . currentUserKey

-- | POST /user/keys
createKey ::
	NewKey ->
	GitHub UserKeyData
createKey = post currentUserKeys

-- | PATCH /user/keys
updateKey ::
	Int ->
	KeyPatch ->
	GitHub UserKeyData
updateKey i = patch (currentUserKey i)

-- | DELETE /user/keys/:id
deleteKey ::
	Int ->
	GitHub ()
deleteKey i = delete . currentUserKey
