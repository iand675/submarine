module GitHub.Internal where
import Network.HTTP.Conduit

get :: FromJSON a => Params -> Path -> GitHub a
put :: (ToJSON a, FromJSON b) => Params -> Path -> a -> GitHub b

post :: 
post'
patch
patch'
delete
delete'