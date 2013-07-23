{-# LANGUAGE QuasiQuotes #-}
module Main where
import Control.Monad.Writer.Strict
import URI.Parser
import URI.Template
import URI.TH
import URI.Types
import System.Exit
import Test.HUnit hiding (test)

{-describe ''Something $ do-}
  {-it "somethings the something." $ do-}
    {-order <- newOrder-}
    {-let order' = addEntry order $ LineItem { item = Item { price = Money USD 1.11 } }-}
    {-expect (total order') `to` Equal $ Money USD 1.11-}

type TestRegistry = Writer [Test]

test :: Assertion -> TestRegistry ()
test t = tell [TestCase t]

suite :: TestRegistry () -> TestRegistry ()
suite t = tell [TestList $ execWriter t]

label :: String -> TestRegistry () -> TestRegistry ()
label n = censor (\l -> [TestLabel n $ TestList l])

runTestRegistry :: TestRegistry () -> IO Counts
runTestRegistry = runTestTT . TestList . execWriter

main = do
  counts <- testRun
  let statusCode = errors counts + failures counts
  if statusCode > 0
    then exitFailure
    else exitSuccess
  where
    testRun = runTestRegistry (parserTests >> quasiQuoterTests >> embedTests)

parserTests = label "Parser Tests" $ suite $ do
  let foo = Variable "foo" Normal
  label "Literal" $ parserTest "foo" $ Literal "foo"
  label "Simple" $ parserTest "{foo}" $ Embed Simple [foo]
  label "Reserved" $ parserTest "{+foo}" $ Embed Reserved [foo]
  label "Fragment" $ parserTest "{#foo}" $ Embed Fragment [foo]
  label "Label" $ parserTest "{.foo}" $ Embed URI.Types.Label [foo]
  label "Path Segment" $ parserTest "{/foo}" $ Embed PathSegment [foo]
  label "Path Parameter" $ parserTest "{;foo}" $ Embed PathParameter [foo]
  label "Query" $ parserTest "{?foo}" $ Embed Query [foo]
  label "Query Continuation" $ parserTest "{&foo}" $ Embed QueryContinuation [foo]
  label "Explode" $ parserTest "{foo*}" $ Embed Simple [Variable "foo" Explode]
  label "Max Length" $ parserTest "{foo:1}" $ Embed Simple [Variable "foo" $ MaxLength 1]
  label "Multiple Variables" $ parserTest "{foo,bar}" $ Embed Simple [Variable "foo" Normal, Variable "bar" Normal]

parserTest :: String -> TemplateSegment -> TestRegistry ()
parserTest t e = test $ parseTemplate "test" t @?= Right [e]

embedTests = label "Embed Tests" $ suite $ do
  label "Literal" $ embedTest "foo" "foo"
  label "Simple" $ embedTest "{foo}" "bar"
  label "Reserved" $ embedTest "{+foo}" "bar"
  label "Fragment" $ embedTest "{#foo}" "#bar"
  label "Label" $ embedTest "{.foo}" ".bar"
  label "Path Segment" $ embedTest "{/foo}" "/bar"
  label "Path Parameter" $ embedTest "{;foo}" ";foo=bar"
  label "Query" $ embedTest "{?foo}" "?foo=bar"
  label "Query Continuation" $ embedTest "{&foo}" "&foo=bar"
  label "Explode" $ embedTest "{foo*}" "bar"
  label "Max Length" $ embedTest "{foo:1}" "b"

embedTestEnv = [("foo", Single "bar")]

embedTest :: String -> String -> TestRegistry ()
embedTest t expect = test $ do
  let (Right tpl) = parseTemplate "test" t
  let rendered = render tpl embedTestEnv
  rendered @?= expect

var :: String
var = "value"

hello :: String
hello = "Hello World!"

path :: String
path = "/foo/bar"

list :: ListElem [String]
list = ListElem ["red", "green", "blue"]
{-keys = [("semi", ";"), ("dot", "."), ("comma", ",")]-}

quasiQuoterTests = label "QuasiQuoter Tests" $ suite $ do
  label "Simple" $ test ([uri|{var}|] @?= "value")
  label "Multiple" $ test ([uri|{var,hello}|] @?= "value,Hello%20World%21")
  {-[uri|{var:3}|] @?= "val"-}
  {-[uri|{var:10}|] @?= "value"-}
  label "List" $ test ([uri|{list}|] @?= "red,green,blue")
  label "Explode List" $ test ([uri|{list*}|] @?= "red,green,blue")
  {-[uri|{keys}|] @?= "semi,%3B,dot,.,comma,%2C"-}
  {-[uri|{keys*}|] @?= "semi=%3B,dot=.,comma=%2C"-}

{-unescaped = do-}
  {-[url|{+path:6}/here|] @?= "/foo/b/here"-}
  {-[url|{+list}|] @?= "red,green,blue"-}
  {-[url|{+list}|] @?= "red,green,blue"-}
  {-[url|{+keys}|] @?= "semi,;,dot,.,comma,,"-}
  {-[url|{+keys}|] @?= "semi=;,dot=.,comma=,"-}

{-fragment = do-}
  {-[url|{#path:6}/here|] @?= "#/foo/b/here"-}
  {-[url|{#list}|] @?= "#red,green,blue"-}
  {-[url|{#list*}|] @?= "#red,green,blue"-}
  {-[url|{#keys}|] @?= "#semi,;,dot,.,comma,,"-}
  {-[url|{#keys*}|] @?= "#semi=;,dot=.,comma=,"-}

{-label = do-}
  {-[url|X{.var:3}|] @?= "X.val"-}
  {-[url|X{.list}|] @?= "X.red,green,blue"-}
  {-[url|X{.list*}|] @?= "X.red.green.blue"-}
  {-[url|X{.keys}|] @?= "X.semi,%3B,dot,.,comma,%2C"-}
  {-[url|X{.keys*}|] @?= "X.semi=%3B.dot=..comma=%2C"-}

{-path = do-}
  {-[url|{/var:1,var}|] @?= "/v/value"-}
  {-[url|{/list}|] @?= "/red,green,blue"-}
  {-[url|{/list*}|] @?= "/red/green/blue"-}
  {-[url|{/list*,path:4}|] @?= "/red/green/blue/%2Ffoo"-}
  {-[url|{/keys}|] @?= "/semi,%3B,dot,.,comma,%2C"-}
  {-[url|{/keys*}|] @?= "/semi=%3B/dot=./comma=%2C"-}

{-pathParams = do-}
  {-[url|{;hello:5}|] @?= ";hello=Hello"-}
  {-[url|{;list}|] @?= ";list=red,green,blue"-}
  {-[url|{;list*}|] @?= ";list=red;list=green;list=blue"-}
  {-[url|{;keys}|] @?= ";keys=semi,%3B,dot,.,comma,%2C"-}
  {-[url|{;keys*}|] @?= ";semi=%3B;dot=.;comma=%2C"-}

{-queryParams = do-}
  {-[url|{?foo}|] @?= "?foo=1"-}
  {-[url|{?foo,bar}|] @?= "?foo=1&bar=2"-}

{-continuedQueryParams = do-}
  {-[url|{&foo}|] @?= "&foo=1"-}
  {-[url|{&foo,bar}|] @?= "&foo=1&bar=2"-}

