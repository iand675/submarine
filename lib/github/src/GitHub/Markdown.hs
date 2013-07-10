module GitHub.Markdown where

--| POST /markdown
renderMarkdown :: MarkdownInput -> GitHub RenderedMarkdown

--| POST /markdown/raw
renderRawMarkdown :: Text -> GitHub Text
