module Main
    ( main
    ) where

import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified Spec.Unit


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "all tests"
    [ Spec.Unit.tests
    ]

