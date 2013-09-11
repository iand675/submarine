module Submarine.Web.Routing where
import Data.HashMap.Strict
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai

data Route = Route
  { route :: Text
  , handlers :: HashMap Method RouteHandler
  }

data RouteHandler = RouteHandler
  { handler :: Request -> IO Response
  , availabilityChecker :: Request -> IO Bool
  }

--routingTable :: [Route] -> Request -> IO Response

