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

data APIError = InvalidJSON | ExceptionalStatusCodeError String

type RequestMiddleware = Request (ResourceT IO) -> Request (ResourceT IO)

runAPIClient :: String -> RequestMiddleware -> APIClient a -> IO (Either APIError a)
runAPIClient base middleware m = withManager $ \man -> do
	r <- parseUrl base
	runEitherT $ runReaderT (fromAPIClient m) $ ClientSettings r man middleware

jsonize :: (FromJSON a) => Response L.ByteString -> APIClient (Response a)
jsonize r = APIClient $ case decode $ responseBody r of
  Nothing -> lift $ left InvalidJSON
  Just jsonResp -> return $ r { responseBody = jsonResp }

data ClientSettings = ClientSettings
  { baseRequest :: Request (ResourceT IO)
  , clientManager :: Manager
  , requestMiddleware :: RequestMiddleware
  }

newtype APIClient a = APIClient { fromAPIClient :: ReaderT ClientSettings (EitherT APIError (ResourceT IO)) a }
	deriving (Functor, Applicative, Monad, MonadIO)

get :: FromJSON a => ByteString -> APIClient (Response a)
get p = APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = req { path = p }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

put :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
put p v = APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { method = "PUT", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

post :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
post p v = APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { method = "POST", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

patch :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
patch p v = APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { method = "PATCH", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

delete :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
delete p v = APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { method = "DELETE", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

