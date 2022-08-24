{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.NaturalNumber.NatGE0 
    ( NatGE0 -- hide constructor
    , divMod
    , isOne
    , isZero
    , mkNatGE0
    , mkOk
    , plus, minus, times                         
    , unNatGE0  
    , nat0, nat1, nat2, nat3, nat4, nat5, nat6, nat7, nat8, nat9, nat10, nat11, nat12, nat13, nat14, nat15, nat16, nat17, nat18, nat19, nat20     
    )
    where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Maybe (fromJust)
import           GHC.Generics (Generic)
import           PlutusTx.Prelude
                    ( otherwise,
                    Bool,
                    Integer,
                    Maybe(..),
                    Eq(..),
                    Ord((>=)),
                    AdditiveGroup((-)),
                    AdditiveSemigroup((+)),
                    MultiplicativeSemigroup((*)),
                    ($),
                    (.))
import qualified PlutusTx.Prelude as TxP (divMod)                   
import qualified PlutusTx
import qualified Prelude as H--askell   
import           Schema (ToSchema) 


newtype NatGE0 = NatGE0 {unNatGE0 :: Integer}
    deriving stock (H.Eq, H.Ord, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''NatGE0    


{-# INLINABLE mkNatGE0 #-}
mkNatGE0 :: Integer -> Maybe NatGE0
mkNatGE0 n 
    | n >= 0 = Just $ NatGE0 n
    | otherwise = Nothing


{-# INLINABLE mkOk #-}
mkOk :: Integer -> NatGE0
mkOk = fromJust . mkNatGE0


{-# INLINABLE plus #-}
plus :: NatGE0 -> NatGE0 -> NatGE0
plus (NatGE0 x) (NatGE0 y) = NatGE0 $ x + y


{-# INLINABLE minus #-}
minus :: NatGE0 -> NatGE0 -> Maybe NatGE0
minus (NatGE0 x) (NatGE0 y) = mkNatGE0 $ x - y


{-# INLINABLE times #-}
times :: NatGE0 -> NatGE0 -> NatGE0
times (NatGE0 x) (NatGE0 y) = NatGE0 $ x * y


{-# INLINABLE divMod #-}
divMod :: NatGE0 -> NatGE0 -> (NatGE0, NatGE0)
divMod (NatGE0 x) (NatGE0 y) = (NatGE0 div, NatGE0 mod)
    where (div, mod) = TxP.divMod x y


{-# INLINABLE nat0 #-}
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
nat0, nat1, nat2, nat3, nat4, nat5, nat6, nat7, nat8, nat9, nat10, nat11, nat12, nat13, nat14, nat15, nat16, nat17, nat18, nat19, nat20 :: NatGE0
nat0 = NatGE0 0
nat1 = NatGE0 1
nat2 = NatGE0 2
nat3 = NatGE0 3
nat4 = NatGE0 4
nat5 = NatGE0 5
nat6 = NatGE0 6
nat7 = NatGE0 7
nat8 = NatGE0 8
nat9 = NatGE0 9
nat10 = NatGE0 10
nat11 = NatGE0 11
nat12 = NatGE0 12
nat13 = NatGE0 13
nat14 = NatGE0 14
nat15 = NatGE0 15
nat16 = NatGE0 16
nat17 = NatGE0 17
nat18 = NatGE0 18
nat19 = NatGE0 19
nat20 = NatGE0 20


{-# INLINABLE isZero #-}
isZero :: NatGE0 -> Bool 
isZero x = x == nat0


{-# INLINABLE isOne #-}
isOne :: NatGE0 -> Bool 
isOne x = x == nat1