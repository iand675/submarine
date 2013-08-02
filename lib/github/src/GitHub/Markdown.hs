module GitHub.Markdown where
import GitHub.Internal

--| POST /markdown
renderMarkdown :: MarkdownInput -> GitHub RenderedMarkdown

--| POST /markdown/raw
renderRawMarkdown :: Text -> GitHub Text
