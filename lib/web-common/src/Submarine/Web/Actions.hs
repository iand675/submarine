module Submarine.Web.Actions where
import Control.Monad.Trans
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Network.HTTP.Types (Status)
import Network.Wai (Request)
import qualified Web.Scotty as S

request :: (MonadTrans m) => m S.ActionM Request
request = lift S.request

reqHeader :: (MonadTrans m) => Text -> m S.ActionM Text
reqHeader = lift . S.reqHeader

body :: (MonadTrans m) => m S.ActionM ByteString
body = lift S.body

param :: (MonadTrans m, S.Parsable a) => Text -> m S.ActionM a
param = lift . S.param

params :: (MonadTrans m) => m S.ActionM [S.Param]
params = lift S.params

jsonData :: (MonadTrans m, FromJSON a) => m S.ActionM a
jsonData = lift S.jsonData

files :: (MonadTrans m) => m S.ActionM [S.File]
files = lift S.files

status :: (MonadTrans m) => Status -> m S.ActionM ()
status = lift . S.status

header :: (MonadTrans m) => Text -> Text -> m S.ActionM ()
header k v = lift $ S.header k v

redirect :: (MonadTrans m) => Text -> m S.ActionM ()
redirect = lift . S.redirect

text :: (MonadTrans m) => Text -> m S.ActionM ()
text = lift . S.text

html :: (MonadTrans m) => Text -> m S.ActionM ()
html = lift . S.html

file :: (MonadTrans m) => FilePath -> m S.ActionM ()
file = lift . S.file

json :: (MonadTrans m, ToJSON a) => a -> m S.ActionM ()
json = lift . S.json

-- source :: (MonadTrans m) => m S.ActionM
-- source = lift . S.source
