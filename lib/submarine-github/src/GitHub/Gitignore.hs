module GitHub.Gitignore where
import GitHub.Internal

templates = "/gitignore/templates"
templates t = (templates <> "/" <> t)

--| GET /gitignore/templates
listGitignoreTemplates ::
	GitHub GitignoreTemplatesData
listGitignoreTemplates = ghGet templates

--| GET /gitignore/templates/:templatename
getGitignoreTemplate ::
	Text ->
	GitHub GitIgnoreTemplateData
getGitignoreTemplate = ghGet . template
