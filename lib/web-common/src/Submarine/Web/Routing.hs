module Submarine.Web.Routing where

data Route = Route
  { route :: Text
  , handlers :: HashMap Method RouteHandler
  }

data RouteHandler = RouteHandler
  { handler :: Request -> IO Response
  , availabilityChecker :: Request -> IO Bool
  }

routingTable :: [Route] -> Request -> IO Response

