module Spec.NaturalNumber
    ( tests
    ) where

import          Test.Tasty ( testGroup, TestTree )
import          Test.Tasty.HUnit ( testCase, (@?=) )

import qualified Lib.NaturalNumber.Extra as Extra
import qualified Lib.NaturalNumber.NatGE0 as N0
import qualified Lib.NaturalNumber.NatGE1 as N1 


tests :: TestTree
tests = testGroup "Tests" [unitTests] 


unitTests :: TestTree
unitTests = testGroup "BoundedInteger" 
    [ testGroup "NatGE1" 
        [ testCase " 1" $ N1.nat1 `N1.plus` N1.nat2  @?= N1.nat3   
        , testCase " 2" $ N1.nat2 `N1.times` N1.nat3  @?= N1.nat6
        , testCase " 3" $ N1.nat8 `N1.minus` N1.nat3  @?= Just N1.nat5     
        , testCase " 4" $ N1.nat3 `N1.minus` N1.nat8  @?= Nothing    
        , testCase " 5" $ N1.nat3 `N1.minus` N1.nat3  @?= Nothing
        , testCase " 6" $ N1.nat8 `N1.divMod` N1.nat4  @?= (N1.nat2, N0.nat0)
        , testCase " 7" $ N1.nat8 `N1.divMod` N1.nat3  @?= (N1.nat2, N0.nat2)          
        , testCase " 8" $ N1.ceilDiv N1.nat1 N1.nat3 @?= N1.nat1   
        , testCase " 9" $ N1.ceilDiv N1.nat8 N1.nat4 @?= N1.nat2      
        , testCase "10" $ N1.ceilDiv N1.nat8 N1.nat3 @?= N1.nat3              
        , testCase "11" $ N1.isOne N1.nat1 @?= True
        , testCase "12" $ N1.isOne N1.nat10 @?= False         
        , testCase "13" $ N1.isZero N1.nat1 @?= False                  
        ]

    , testGroup "NatGE0"
        [ testCase " 1" $ N0.nat1 `N0.plus` N0.nat2  @?= N0.nat3   
        , testCase " 2" $ N0.nat0 `N0.plus` N0.nat2  @?= N0.nat2  
        , testCase " 3" $ N0.nat3 `N0.times` N0.nat2  @?= N0.nat6 
        , testCase " 4" $ N0.nat3 `N0.times` N0.nat0  @?= N0.nat0 
        , testCase " 5" $ N0.nat8 `N0.minus` N0.nat3  @?= Just N0.nat5
        , testCase " 6" $ N0.nat3 `N0.minus` N0.nat8  @?= Nothing
        , testCase " 7" $ N0.nat3 `N0.minus` N0.nat3  @?= Just N0.nat0
        , testCase " 8" $ N0.nat8 `N0.divMod` N0.nat4  @?= (N0.nat2, N0.nat0)
        , testCase " 9" $ N0.nat8 `N0.divMod` N0.nat3  @?= (N0.nat2, N0.nat2)
        , testCase "10" $ N0.isOne N0.nat1 @?= True
        , testCase "11" $ N0.isOne N0.nat10 @?= False      
        , testCase "12" $ N0.isZero N0.nat0 @?= True        
        , testCase "13" $ N0.isZero N0.nat10 @?= False          
        ]

    , testGroup "Extra"
        [ testCase " 1" $ Extra.natGE1ToGE0 N1.nat3 @?= N0.nat3
        , testCase " 2" $ Extra.natGE1ToGE0 N1.nat1 @?= N0.nat1
        , testCase " 3" $ Extra.natGE0ToGE1 N0.nat1 @?= Just N1.nat1
        , testCase " 4" $ Extra.natGE0ToGE1 N0.nat0 @?= Nothing
        , testCase " 5" $ N1.nat1 `Extra.plus` N0.nat2  @?= N1.nat3   
        , testCase " 6" $ N1.nat1 `Extra.plus` N0.nat0  @?= N1.nat1  
        , testCase " 7" $ N1.nat3 `Extra.times` N0.nat2  @?= N0.nat6  
        , testCase " 8" $ N1.nat3 `Extra.times` N0.nat0  @?= N0.nat0                    
        , testCase " 9" $ Extra.ceilDiv N0.nat0 N1.nat4 @?= N0.nat0  
        , testCase "10" $ Extra.ceilDiv N0.nat1 N1.nat3 @?= N0.nat1   
        , testCase "11" $ Extra.ceilDiv N0.nat8 N1.nat4 @?= N0.nat2
        , testCase "12" $ Extra.ceilDiv N0.nat8 N1.nat3 @?= N0.nat3                 
        ]
    ]