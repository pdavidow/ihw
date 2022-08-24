{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.NaturalNumber.Extra
    ( ceilDiv
    , plus   
    , times      
    , natGE0ToGE1   
    , natGE1ToGE0       
    )
    where

import           Data.Maybe (fromJust)
import           PlutusTx.Prelude ( Maybe, otherwise, ($), (.), AdditiveSemigroup((+)) ) 
import           Lib.NaturalNumber.NatGE0 as N0 ( NatGE0(..), isZero )
import qualified Lib.NaturalNumber.NatGE0 as N0 (mkOk, times)
import           Lib.NaturalNumber.NatGE1 as N1 ( NatGE1(..), mkNatGE1, unNatGE1 ) 
import qualified Lib.NaturalNumber.NatGE1 as N1 ( ceilDiv, mkOk )     


{-# INLINABLE natGE1ToGE0 #-}
natGE1ToGE0 :: NatGE1 -> NatGE0
natGE1ToGE0 = N0.mkOk . unNatGE1 


{-# INLINABLE natGE0ToGE1 #-}
natGE0ToGE1 :: NatGE0 -> Maybe NatGE1
natGE0ToGE1 = mkNatGE1 . unNatGE0 


{-# INLINABLE plus #-}
plus :: NatGE1 -> NatGE0 -> NatGE1
plus p n = N1.mkOk $ unNatGE1 p + unNatGE0 n


{-# INLINABLE times #-}
times :: NatGE1 -> NatGE0 -> NatGE0
times p n = natGE1ToGE0 p `N0.times` n


{-# INLINABLE ceilDiv #-}
ceilDiv :: NatGE0 -> NatGE1 -> NatGE0
ceilDiv numerator denominator 
    | N0.isZero numerator = numerator
    | otherwise = natGE1ToGE0 $ N1.ceilDiv posNumerator denominator
    where posNumerator = fromJust $ natGE0ToGE1 numerator -- safe partial
