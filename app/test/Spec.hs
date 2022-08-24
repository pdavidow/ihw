module Main
    ( main
    ) where

import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified Spec.Auction.Unit
import qualified Spec.NaturalNumber
-- import qualified Spec.NEPosValue
import qualified Spec.PosRational
import qualified Spec.PosProperRational
import qualified Spec.PosValue


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "all tests"
    [ Spec.Auction.Unit.tests
    , Spec.NaturalNumber.tests
    -- , Spec.NEPosValue.tests    
    , Spec.PosRational.tests
    , Spec.PosProperRational.tests
    , Spec.PosValue.tests    
    ]

