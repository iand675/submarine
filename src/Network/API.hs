{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.API where
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString as B
import Network.HTTP.Conduit

class ToRouteParameters a where
	toParams :: a -> [(ByteString, Maybe ByteString)]

class ParameterEncodable a where
	paramEncode :: a -> Maybe ByteString

instance ParameterEncodable a => ParameterEncodable (Maybe a) where
	paramEncode = paramEncode

instance ParameterEncodable a => ParameterEncodable [a] where
	paramEncode = Just . B.intercalate "," . catMaybes . map paramEncode

instance ParameterEncodable Int where
	paramEncode = Just . encodeUtf8 . T.pack . show

instance ParameterEncodable T.Text where
	paramEncode = Just . encodeUtf8

runAPIClient :: String -> APIClient a -> IO a
runAPIClient base m = withManager $ \man -> do
	r <- parseUrl base
	runReaderT (fromAPIClient m) (r, man)

newtype APIClient a = APIClient { fromAPIClient :: ReaderT (Request (ResourceT IO), Manager) (ResourceT IO) a }
	deriving (Functor, Applicative, Monad)

get :: FromJSON a => T.Text -> APIClient (Maybe a)
get t = APIClient $ do
	(r, m) <- ask
	res <- lift $ httpLbs r m
	return $ decode $ responseBody res

put :: (ToJSON a, FromJSON b) => T.Text -> a -> APIClient b
put = undefined

post :: (ToJSON a, FromJSON b) => T.Text -> a -> APIClient b
post = undefined

patch :: (ToJSON a, FromJSON b) => T.Text -> a -> APIClient b
patch = undefined

delete :: (ToJSON a, FromJSON b) => T.Text -> a -> APIClient b
delete = undefined
