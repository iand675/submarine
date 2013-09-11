module GitHub.Search where
import GitHub.Internal

--| GET /legacy/issues/search/:owner/:repository/:state/:keyword
searchIssues ::
	OwnerName ->
	RepoName ->
	IssueState ->
	Text ->
	GitHub IssuesSearchResultsData

--| GET /legacy/repos/search/:keyword
searchRepositories ::
	Text ->
	Language ->
	Int ->
	RepoSortBy ->
	SortOrder ->
	GitHub RepositoriesSearchResultsData

--| GET /legacy/user/search/:keyword
searchUsers ::
	Text ->
	Int ->
	UserSortBy ->
	SortOrder ->
	GitHub UsersSearchResultsData
