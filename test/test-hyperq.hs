-- http://lambda.jstolarek.com/2012/10/code-testing-in-haskell/
module Main (
    main
 ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Hyperq.Dot
import Hyperq.EchoBackground
import TestUtils

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests =
  [
    testGroup "Dot Graph utilities"
    [
       testWithProvider "String Dot spec to node list" testImportDotNode dataImportDotNode,
       testWithProvider "String Dot spec to channel list" testImportDotChan dataImportDotChan
    ],

    testGroup "RingBuffer testing"
    [
       testCase "echo using ringbuffer" testEchoBackground
    ]

  ]

testImportDotNode :: (String, [String]) -> Assertion
testImportDotNode (s, expected) = expected @=? (nodeList . importDot) s

dataImportDotNode :: [(String, [String])]
dataImportDotNode = [("digraph G {\nnode [label=\"\\N\"];\nnode [style=filled, color=\"#1f3950\",fontcolor=\"#eeeeee\",shape=box];\ncontroller -> stdin [color=\"#aaaaaa\", dir=back]\ncontroller -> stdout [color=\"#aaaaaa\"]}",["controller","stdin","stdout"])]

testImportDotChan :: (String, [(String, String, String)]) -> Assertion
testImportDotChan (s, expected) = expected @=? concatMap commChan (comm $ importDot s)

dataImportDotChan :: [(String, [(String, String, String)])]
dataImportDotChan = [("digraph G {\nnode [label=\"\\N\"];\nnode [style=filled, color=\"#1f3950\",fontcolor=\"#eeeeee\",shape=box];\ncontroller -> stdin [color=\"#aaaaaa\", dir=back]\ncontroller -> stdout [color=\"#aaaaaa\"]}",[("controller","stdin","Read"),("controller","stdout","Write")])]

testEchoBackground :: Assertion
testEchoBackground = do
    s <- runString "12345q"
    "12345q" @=? s
