module Main
    ( main
    ) where

import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified Spec.Auction.Unit


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "all tests"
    [ Spec.Auction.Unit.tests  
    ]

