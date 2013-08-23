module TestSuiteAdapter where
import Distribution.TestSuite
import qualified Test.Hspec.Core as H
import Test.QuickCheck (stdArgs)

specToTests :: H.SpecM () -> [Test]
specToTests s = let specTrees = H.runSpecM s in map convertToTest specTrees
 where
   convertToTest :: H.SpecTree -> Test
   convertToTest (H.SpecGroup name subSpecs) = Group name True (map convertToTest subSpecs)
   convertToTest (H.SpecItem something specName specF) = Test $ TestInstance
     { run = fmap convertResult $ specF defaultParams
     , name = specName
     , tags = []
     , options = []
     , setOption = \k v -> Left "Specs don't support options"
     }
   defaultParams = H.Params stdArgs $ \(current, total) -> do
     undefined
   convertResult r = Finished $ case r of
     H.Success -> Pass
     H.Pending mstr -> Fail $ maybe "(pending)" id mstr
     H.Fail str -> Fail str

