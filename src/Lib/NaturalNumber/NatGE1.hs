{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.NaturalNumber.NatGE1 
    ( NatGE1 -- hide constructor
    , ceilDiv
    , divMod
    , isOne
    , isZero       
    , mkOk
    , mkNatGE1
    , plus, minus, times                          
    , unNatGE1   
    , nat1, nat2, nat3, nat4, nat5, nat6, nat7, nat8, nat9, nat10, nat11, nat12, nat13, nat14, nat15, nat16, nat17, nat18, nat19, nat20
    )
    where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Maybe (fromJust)
import           GHC.Generics (Generic)
import           PlutusTx.Prelude
                    ( otherwise,
                    Bool(False),
                    Integer,
                    Maybe(..),
                    Eq(..),
                    Ord((>=)),
                    ($),
                    (.),
                    const,
                    AdditiveGroup((-)),
                    AdditiveSemigroup((+)),
                    MultiplicativeSemigroup((*)) )
import qualified PlutusTx.Prelude as TxP (divMod)
import qualified PlutusTx
import qualified Prelude as H--askell   
import           Schema (ToSchema) 

import qualified Lib.NaturalNumber.NatGE0 as N0


newtype NatGE1 = NatGE1 {unNatGE1 :: Integer}
    deriving stock (H.Eq, H.Ord, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''NatGE1    


{-# INLINABLE mkNatGE1 #-}
mkNatGE1 :: Integer -> Maybe NatGE1
mkNatGE1 n 
    | n >= 1 = Just $ NatGE1 n
    | otherwise = Nothing


{-# INLINABLE mkOk #-}
mkOk :: Integer -> NatGE1
mkOk = fromJust . mkNatGE1 


{-# INLINABLE plus #-}
plus :: NatGE1 -> NatGE1 -> NatGE1
plus (NatGE1 x) (NatGE1 y) = NatGE1 $ x + y


{-# INLINABLE minus #-}
minus :: NatGE1 -> NatGE1 -> Maybe NatGE1
minus (NatGE1 x) (NatGE1 y) = mkNatGE1 $ x - y


{-# INLINABLE times #-}
times :: NatGE1 -> NatGE1 -> NatGE1
times (NatGE1 x) (NatGE1 y) = NatGE1 $ x * y


{-# INLINABLE divMod #-}
divMod :: NatGE1 -> NatGE1 -> (NatGE1, N0.NatGE0)
divMod numer denom = (NatGE1 div, N0.mkOk mod)
    where (div, mod) = TxP.divMod (unNatGE1 numer) (unNatGE1 denom)


{-# INLINABLE nat1 #-}
{-# INLINABLE nat2 #-}
{-# INLINABLE nat3 #-}
{-# INLINABLE nat4 #-}
{-# INLINABLE nat5 #-}
{-# INLINABLE nat6 #-}
{-# INLINABLE nat7 #-}
{-# INLINABLE nat8 #-}
{-# INLINABLE nat9 #-}
{-# INLINABLE nat10 #-}
{-# INLINABLE nat11 #-}
{-# INLINABLE nat12 #-}
{-# INLINABLE nat13 #-}
{-# INLINABLE nat14 #-}
{-# INLINABLE nat15 #-}
{-# INLINABLE nat16 #-}
{-# INLINABLE nat17 #-}
{-# INLINABLE nat18 #-}
{-# INLINABLE nat19 #-}
{-# INLINABLE nat20 #-}
nat1, nat2, nat3, nat4, nat5, nat6, nat7, nat8, nat9, nat10, nat11, nat12, nat13, nat14, nat15, nat16, nat17, nat18, nat19, nat20 :: NatGE1
nat1 = NatGE1 1
nat2 = NatGE1 2
nat3 = NatGE1 3
nat4 = NatGE1 4
nat5 = NatGE1 5
nat6 = NatGE1 6
nat7 = NatGE1 7
nat8 = NatGE1 8
nat9 = NatGE1 9
nat10 = NatGE1 10
nat11 = NatGE1 11
nat12 = NatGE1 12
nat13 = NatGE1 13
nat14 = NatGE1 14
nat15 = NatGE1 15
nat16 = NatGE1 16
nat17 = NatGE1 17
nat18 = NatGE1 18
nat19 = NatGE1 19 
nat20 = NatGE1 20


{-# INLINABLE isZero #-}
isZero :: NatGE1 -> Bool 
isZero = const False


{-# INLINABLE isOne #-}
isOne :: NatGE1 -> Bool 
isOne x = x == nat1


{-# INLINABLE ceilDiv #-}
ceilDiv :: NatGE1 -> NatGE1 -> NatGE1
ceilDiv numerator denominator 
    | N0.isZero mod = div
    | otherwise =  div `plus` nat1
    where (div, mod) = divMod numerator denominator