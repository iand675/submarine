module Database.Redis.JSON where

import Data.Aeson

require :: Maybe a -> Redis a
get :: FromJSON a => ByteString -> Redis a
get = get >=> (require . decode)

set :: ToJSON a => ByteString -> a -> Redis R.Status
set k = set k . encode v

hGet :: FromJSON a => ByteString -> ByteString -> Redis a
hGet k f = hGet k f >>= require . decode

hGetAll :: FromJSON a => ByteString -> Redis a
hSet :: ToJSON a => ByteString -> ByteString -> a -> Redis Bool
hSetNX :: ToJSON a => ByteString -> ByteString -> a -> Redis Bool
hMSet :: ToJSON a => ByteString -> a -> Redis R.Status
hMGet :: ByteString -> [ByteString] -> Redis [Maybe Object]
hVals :: ByteString -> Redis [Object]

