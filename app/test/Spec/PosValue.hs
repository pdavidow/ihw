module Spec.PosValue where

import          Data.Maybe ( isJust, isNothing )
import          Test.Tasty ( testGroup, TestTree )
import          Test.Tasty.HUnit ( testCase, (@?=) )

import           Plutus.V1.Ledger.Ada ( adaSymbol, adaToken )
import qualified Plutus.V1.Ledger.Value as V

import qualified Lib.PosValue as PV
import qualified Lib.NaturalNumber.NatGE1 as N1
import           Lib.PosProperRational as PR ( half )
import           Utility (adaAssetClass) 


tests :: TestTree
tests = testGroup "Tests" [unitTests] 


unitTests :: TestTree
unitTests = testGroup "PosValue" 
    [ testGroup "mkPosValue" 
        [ testCase "1" $ isJust (PV.mkPosValue mempty) @?= True   
        , testCase "2" $ isNothing (PV.mkPosValue (V.assetClassValue adaAssetClass 0)) @?= True   
        , testCase "3" $ isNothing (PV.mkPosValue (V.assetClassValue adaAssetClass (-8))) @?= True         
        , testCase "4" $ isJust (PV.mkPosValue (V.assetClassValue adaAssetClass 8)) @?= True            
        ]            
    ]

    