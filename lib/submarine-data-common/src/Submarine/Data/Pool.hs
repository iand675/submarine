module Submarine.Data.Pool where
import Data.Pool

poolDefaults :: IO a -> (a -> IO ()) -> IO (Pool a)
poolDefaults create destroy = createPool create destroy 4 5 200