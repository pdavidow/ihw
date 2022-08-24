module Spec.PosRational
    ( tests
    ) where

import          Test.Tasty ( testGroup, TestTree )
import          Test.Tasty.HUnit ( testCase, (@?=) )

import qualified Data.List.NonEmpty as NE

import qualified Lib.NaturalNumber.NatGE1 as N1
import qualified Lib.PosRational as R


tests :: TestTree
tests = testGroup "Tests" [unitTests] 


unitTests :: TestTree
unitTests = testGroup "PosRational" 
    [ testGroup "plus" 
        [ testCase "1" $ R.half `R.plus` R.half @?= R.whole
        , testCase "2" $ R.half `R.plus` R.quarter @?= R.quarter3
        , testCase "3" $ R.quarter `R.plus` R.eighth @?= R.eighth3           
        , testCase "4" $ R.half `R.plus` R.quarter3 @?= R.mkPosRational (N1.mkOk 125) (N1.mkOk 100)         
        ]

    , testGroup "minus" 
        [ testCase "1" $ R.half `R.minus` R.half @?= Nothing
        , testCase "2" $ R.half `R.minus` R.quarter @?= Just R.quarter
        , testCase "3" $ R.quarter `R.minus` R.half @?= Nothing           
        , testCase "4" $ R.half `R.minus` R.quarter3 @?= Nothing         
        ]

    , testGroup "times"
        [ testCase "1" $ R.half `R.times` R.half @?= R.quarter
        , testCase "2" $ R.half `R.times` R.quarter @?= R.eighth        
        , testCase "3" $ R.half `R.times` R.quarter3 @?= R.eighth3   
        , testCase "4" $ R.mkPosRational N1.nat3 N1.nat2 `R.times` R.mkPosRational N1.nat5 N1.nat3 @?= R.mkPosRational N1.nat15 N1.nat6 
        ] 

    , testGroup "sum"
        [ testCase "1" $ R.sum (NE.fromList [R.half, R.half]) @?= R.whole
        , testCase "2" $ R.sum (NE.fromList [R.third, R.third]) @?= R.third2
        , testCase "3" $ R.sum (NE.fromList [R.whole, R.half, R.quarter]) @?= R.mkPosRational (N1.mkOk 175) (N1.mkOk 100)  
        ]   

    , testGroup "inverse"
        [ testCase "1" $ R.inverse N1.nat1 @?= R.whole
        , testCase "2" $ R.inverse N1.nat3 @?= R.third
        , testCase "3" $ R.inverse N1.nat19 @?= R.mkPosRational N1.nat1 N1.nat19 
        ] 

    , testGroup "posTimesBasic"
        [ testGroup "Never"
            [ testCase "1" $ R.posTimesBasic R.Never N1.nat3 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat1  
            , testCase "2" $ R.posTimesBasic R.Never N1.nat7 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat3         
            , testCase "3" $ R.posTimesBasic R.Never N1.nat6 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat2     
            , testCase "4" $ R.posTimesBasic R.Never N1.nat5 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat2        
            , testCase "5" $ R.posTimesBasic R.Never N1.nat4 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat1  
            , testCase "6" $ R.posTimesBasic R.Never (N1.mkOk 100) (R.mkPosRational N1.nat1 $ N1.mkOk 25) @?= N1.nat4        
            , testCase "7" $ R.posTimesBasic R.Never N1.nat3 R.third @?= N1.nat1   
            ]     

        , testGroup "Always"
            [ testCase "1" $ R.posTimesBasic R.Always N1.nat3 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat2 
            , testCase "2" $ R.posTimesBasic R.Always N1.nat7 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat4         
            , testCase "3" $ R.posTimesBasic R.Always N1.nat6 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat3     
            , testCase "4" $ R.posTimesBasic R.Always N1.nat5 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat3        
            , testCase "5" $ R.posTimesBasic R.Always N1.nat4 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat2  
            , testCase "6" $ R.posTimesBasic R.Always (N1.mkOk 100) (R.mkPosRational N1.nat1 $ N1.mkOk 25) @?= N1.nat5        
            , testCase "7" $ R.posTimesBasic R.Always N1.nat3 R.third @?= N1.nat2   
            ]          

        , testGroup "IfNeeded"
            [ testCase "1" $ R.posTimesBasic R.IfNeeded N1.nat3 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat2 
            , testCase "2" $ R.posTimesBasic R.IfNeeded N1.nat7 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat3       
            , testCase "3" $ R.posTimesBasic R.IfNeeded N1.nat6 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat3     
            , testCase "4" $ R.posTimesBasic R.IfNeeded N1.nat5 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat3        
            , testCase "5" $ R.posTimesBasic R.IfNeeded N1.nat4 (R.mkPosRational N1.nat3 N1.nat7) @?= N1.nat2  
            , testCase "6" $ R.posTimesBasic R.IfNeeded (N1.mkOk 100) (R.mkPosRational N1.nat1 $ N1.mkOk 25) @?= N1.nat4        
            , testCase "7" $ R.posTimesBasic R.IfNeeded N1.nat3 R.third @?= N1.nat1
            ]                   
        ]
    ]

        