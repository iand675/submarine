module GitHub.Gitignore where

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
