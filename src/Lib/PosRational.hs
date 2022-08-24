{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.PosRational
    ( CeilingMode(..)
    , PosRational -- hide constructor
    , denominator
    , inverse
    , mkPosRational
    , numerator
    , plus, minus, times     
    , sum
    , posTimes
    , posTimesBasic
    , unPosRational
    , whole, half, third, third2, quarter, quarter3, eighth, eighth3, eighth5, eighth7, tenth, twelfth
    )
    where

import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.List.NonEmpty as NE
import           GHC.Generics (Generic)

import           PlutusTx.Prelude
                    ( otherwise,
                    Bool(False, True),
                    Integer,
                    Maybe(..),
                    Eq(..),
                    Ord((>)),
                    ($),
                    (.),
                    (%),
                    AdditiveGroup((-)),
                    AdditiveMonoid(zero),
                    AdditiveSemigroup((+)),
                    MultiplicativeSemigroup((*)),
                    Rational )
import qualified PlutusTx
import qualified PlutusTx.Ratio as TxR
import qualified Prelude as H--askell   
import           Schema (ToSchema) 

import           Lib.NaturalNumber.NatGE1 as N1 ( NatGE1(..) ) 
import qualified Lib.NaturalNumber.NatGE1 as N1 ( mkOk, nat1 ) 


newtype PosRational = PosRational {unPosRational :: Rational} 
    deriving stock (H.Eq, H.Ord, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''PosRational 


data CeilingMode 
    = Never
    | Always 
    | IfNeeded
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq CeilingMode where
    {-# INLINABLE (==) #-}
    Never == Never = True 
    Always == Always = True 
    IfNeeded == IfNeeded = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''CeilingMode [('Never, 0), ('Always, 1), ('IfNeeded, 2)]
PlutusTx.makeLift ''CeilingMode 


{-# INLINABLE mkPosRational #-}
mkPosRational :: NatGE1 -> NatGE1 -> PosRational
mkPosRational numer denom = PosRational $ unNatGE1 numer % unNatGE1 denom


{-# INLINABLE numerator #-}
numerator :: PosRational -> Integer
numerator = TxR.numerator . unPosRational


{-# INLINABLE denominator #-}
denominator :: PosRational -> Integer
denominator = TxR.denominator . unPosRational


{-# INLINABLE plus #-}
plus :: PosRational -> PosRational -> PosRational
plus (PosRational x) (PosRational y) = PosRational $ x + y


{-# INLINABLE minus #-}
minus :: PosRational -> PosRational -> Maybe PosRational
minus (PosRational x) (PosRational y)
    | result > zero = Just $ PosRational result
    | otherwise =  Nothing
    where result = x - y


{-# INLINABLE times #-}
times :: PosRational -> PosRational -> PosRational
times (PosRational x) (PosRational y) = PosRational $ x * y


{-# INLINABLE posTimes #-}
posTimes :: NatGE1 -> PosRational -> NatGE1
posTimes = posTimesBasic IfNeeded 


{-# INLINABLE posTimesBasic #-}
posTimesBasic :: CeilingMode -> NatGE1 -> PosRational -> NatGE1
posTimesBasic mode pos rat = 
    let
        (d, m) = TxR.divMod (unNatGE1 pos * numerator rat) $ denominator rat 
    in
        N1.mkOk $ case mode of
            Never -> d
            Always -> d + 1
            IfNeeded -> if m == 0 then d else d + 1


sum :: NE.NonEmpty PosRational -> PosRational
sum = H.foldr1 plus


inverse :: NatGE1 -> PosRational
inverse = mkPosRational N1.nat1
    

whole, half, third, third2, quarter, quarter3, eighth, eighth3, eighth5, eighth7, tenth, twelfth :: PosRational
whole    = PosRational (1 % 1) 
half     = PosRational (1 % 2) 
third    = PosRational (1 % 3) 
third2   = PosRational (2 % 3) 
quarter  = PosRational (1 % 4) 
quarter3 = PosRational (3 % 4) 
eighth   = PosRational (1 % 8) 
eighth3  = PosRational (3 % 8) 
eighth5  = PosRational (5 % 8) 
eighth7  = PosRational (7 % 8)             
tenth    = PosRational (1 % 10)   
twelfth  = PosRational (1 % 12) 