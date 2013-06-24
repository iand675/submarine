module Network.API where
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString as B

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
