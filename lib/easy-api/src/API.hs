{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module API where
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit

data APIError = InvalidJSON | AuthorizationError

runAPIClient :: String -> APIClient a -> IO (Either APIError a)
runAPIClient base m = withManager $ \man -> do
	r <- parseUrl base
	runEitherT $ runReaderT (fromAPIClient m) (r, man)

jsonize :: (FromJSON a) => Response L.ByteString -> APIClient (Response a)
jsonize r = APIClient $ case decode $ responseBody r of
  Nothing -> lift $ left InvalidJSON
  Just jsonResp -> return $ r { responseBody = jsonResp }

newtype APIClient a = APIClient { fromAPIClient :: ReaderT (Request (ResourceT IO), Manager) (EitherT APIError (ResourceT IO)) a }
	deriving (Functor, Applicative, Monad, MonadIO)

get :: FromJSON a => ByteString -> APIClient (Response (Maybe a))
get p = APIClient $ do
  (req, man) <- ask
  let r = req { path = p }
  resp <- lift $ lift $ httpLbs req man
  fromAPIClient $ jsonize resp


put :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
put p v = APIClient $ do
  (req, man) <- ask
  let r = req { method = "PUT", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp


post :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
post p v = APIClient $ do
  (req, man) <- ask
  let r = req { method = "POST", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

patch :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
patch p v = APIClient $ do
  (req, man) <- ask
  let r = req { method = "PATCH", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

delete :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
delete p v = APIClient $ do
  (req, man) <- ask
  let r = req { method = "DELETE", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

