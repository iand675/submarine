module GitHub.Repositories.Hooks where
import GitHub.Internal

hooks o r = ownerRepo o r <> "/hooks"
hook o r i = hooks o r <> "/" <> i

--| GET /repos/:owner/:repo/hooks
listHooks ::
	OwnerName ->
	RepoName ->
	GitHub HooksData
listHooks o r = ghGet [] (hooks o r)

--| GET /repos/:owner/:repo/hooks/:id
getHook ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub HookData
getHook o r i = ghGet [] (hook o r i)

--| POST /repos/:owner/:repo/hooks
createHook ::
	OwnerName ->
	RepoName ->
	NewHook ->
	GitHub HookData
createHook o r = ghPost (hooks o r)

--| PATCH /repos/:owner/:repo/hooks/:id
editHook ::
	OwnerName ->
	RepoName ->
	Int ->
	HookPatch ->
	GitHub HookData
editHook o r i = ghPatch (hook o r i)

--| POST /repos/:owner/:repo/hooks/:id/test
testPushHook ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub ()
testPushHook o r i = ghPost (hook o r i <> "/test")

--| DELETE /repos/:owner/:repo/hooks/:id
deleteHook ::
	OwnerName ->
	RepoName ->
	Int ->
	GitHub ()
deleteHook o r i = ghDelete (hook o r i)

