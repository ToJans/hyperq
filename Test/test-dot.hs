-- http://lambda.jstolarek.com/2012/10/code-testing-in-haskell/
module Main (
    main
 ) where


import Test.Framework


import Test.Utils
import Test.HUnit
import Dot

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests =
  [
    testGroup "Dot Graph utilities"
    [
       testWithProvider "String Dot spec to node list" testImportDot dataImportDot 
    ]
  ]

testImportDot :: (String, [String]) -> Assertion
testImportDot (s, expected) = expected @=? (nodeList . importDot) s

dataImportDot :: [(String, [String])]
dataImportDot = [("digraph G {\nnode [label=\"\\N\"];\nnode [style=filled, color=\"#1f3950\",fontcolor=\"#eeeeee\",shape=box];\ncontroller -> stdin [color=\"#aaaaaa\", dir=back]\ncontroller -> stdout [color=\"#aaaaaa\"]}",["controller","stdin","stdout"])]
