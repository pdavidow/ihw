module Main
    ( main
    ) where

import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified Spec.Auction.Unit
-- import qualified Spec.Auction.Trace


main :: IO ()
main = defaultMain tests

-- main :: IO ()
-- main = Spec.Auction.Trace.test1


tests :: TestTree
tests = testGroup "all tests"
    [ Spec.Auction.Unit.tests  
    ]

