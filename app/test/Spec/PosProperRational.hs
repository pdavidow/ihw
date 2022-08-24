module Spec.PosProperRational
    ( tests
    ) where

import          Test.Tasty ( testGroup, TestTree )
import          Test.Tasty.HUnit ( testCase, (@?=) )

import qualified Data.List.NonEmpty as NE

import qualified Lib.NaturalNumber.NatGE1 as N1 
import qualified Lib.PosRational as R
import qualified Lib.PosProperRational as PR


tests :: TestTree
tests = testGroup "Tests" [unitTests] 


unitTests :: TestTree
unitTests = testGroup "PosProperRational" 
    [ testGroup "plus" 
        [ testCase "1" $ PR.half `PR.plus` PR.half @?= R.whole
        , testCase "2" $ PR.half `PR.plus` PR.quarter @?= R.quarter3
        , testCase "3" $ PR.quarter `PR.plus` PR.eighth @?= R.eighth3           
        , testCase "4" $ PR.half `PR.plus` PR.quarter3 @?= R.mkPosRational (N1.mkOk 125) (N1.mkOk 100)       
        ]

    , testGroup "minus" 
        [ testCase "1" $ PR.half `PR.minus` PR.half @?= Nothing
        , testCase "2" $ PR.half `PR.minus` PR.quarter @?= Just PR.quarter
        , testCase "3" $ PR.quarter `PR.minus` PR.half @?= Nothing           
        , testCase "4" $ PR.half `PR.minus` PR.quarter3 @?= Nothing         
        ]

    , testGroup "times"
        [ testCase "1" $ PR.half `PR.times` PR.half @?= PR.quarter
        , testCase "2" $ PR.half `PR.times` PR.quarter @?= PR.eighth        
        , testCase "3" $ PR.half `PR.times` PR.quarter3 @?= PR.eighth3   
        ] 

    , testGroup "sum"
        [ testCase "1" $ PR.sum (NE.fromList [PR.half, PR.half]) @?= R.whole
        , testCase "2" $ PR.sum (NE.fromList [PR.third, PR.third]) @?= R.third2
        ]   

    , testGroup "subtractFrom1"
        [ testCase "1" $ PR.subtractFrom1 PR.half @?= PR.half
        , testCase "2" $ PR.subtractFrom1 PR.third2 @?= PR.third
        ]  
    ]

        