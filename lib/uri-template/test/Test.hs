module URI where
import Network.URI.Template
import Test.HUnit.Base

main = runTests $ TestList
  [ TestLabel "simple"  simple
  , TestLabel "unescaped" unescaped
  , TestLabel "fragment" fragment
  , TestLabel "label" label
  , path
  , pathParams
  , queryParams
  , continuedQueryParams
  ]

var = "value"
hello = "Hello World!"
path = "/foo/bar"
list = ["red", "green", "blue"]
keys = [("semi", ";"), ("dot", "."), ("comma", ",")]

simple = do
  [url|{var}|] @?= "value"
  [url|{var,hello}|] @?= "value,Hello%20World%21"
  [url|{var:3}|] @?= "val"
  [url|{var:10}|] @?= "value"
  [url|{list}|] @?= "red,green,blue"
  [url|{list*}|] @?= "red,green,blue"
  [url|{keys}|] @?= "semi,%3B,dot,.,comma,%2C"
  [url|{keys*}|] @?= "semi=%3B,dot=.,comma=%2C"

unescaped = do
  [url|{+path:6}/here|] @?= "/foo/b/here"
  [url|{+list}|] @?= "red,green,blue"
  [url|{+list}|] @?= "red,green,blue"
  [url|{+keys}|] @?= "semi,;,dot,.,comma,,"
  [url|{+keys}|] @?= "semi=;,dot=.,comma=,"

fragment = do
  [url|{#path:6}/here|] @?= "#/foo/b/here"
  [url|{#list}|] @?= "#red,green,blue"
  [url|{#list*}|] @?= "#red,green,blue"
  [url|{#keys}|] @?= "#semi,;,dot,.,comma,,"
  [url|{#keys*}|] @?= "#semi=;,dot=.,comma=,"

label = do
  [url|X{.var:3}|] @?= "X.val"
  [url|X{.list}|] @?= "X.red,green,blue"
  [url|X{.list*}|] @?= "X.red.green.blue"
  [url|X{.keys}|] @?= "X.semi,%3B,dot,.,comma,%2C"
  [url|X{.keys*}|] @?= "X.semi=%3B.dot=..comma=%2C"

path = do
  [url|{/var:1,var}|] @?= "/v/value"
  [url|{/list}|] @?= "/red,green,blue"
  [url|{/list*}|] @?= "/red/green/blue"
  [url|{/list*,path:4}|] @?= "/red/green/blue/%2Ffoo"
  [url|{/keys}|] @?= "/semi,%3B,dot,.,comma,%2C"
  [url|{/keys*}|] @?= "/semi=%3B/dot=./comma=%2C"

pathParams = do
  [url|{;hello:5}|] @?= ";hello=Hello"
  [url|{;list}|] @?= ";list=red,green,blue"
  [url|{;list*}|] @?= ";list=red;list=green;list=blue"
  [url|{;keys}|] @?= ";keys=semi,%3B,dot,.,comma,%2C"
  [url|{;keys*}|] @?= ";semi=%3B;dot=.;comma=%2C"

queryParams = do
  [url|{?foo}|] @?= "?foo=1"
  [url|{?foo,bar}|] @?= "?foo=1&bar=2"

continuedQueryParams = do
  [url|{&foo}|] @?= "&foo=1"
  [url|{&foo,bar}|] @?= "&foo=1&bar=2"

