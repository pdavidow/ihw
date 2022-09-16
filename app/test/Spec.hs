module Main
    ( main
    ) where

import Test.Tasty ( defaultMain, testGroup )

import qualified Spec.Auction.Unit


main :: IO ()
main = defaultMain $ testGroup "all tests"
    [ Spec.Auction.Unit.tests  
    ]