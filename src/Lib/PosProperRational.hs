{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.PosProperRational
    ( PosProperRational -- hide constructor
    , mkPosProperRational
    , denominator
    , minus
    , mkOk
    , numerator
    , plus
    , posTimes
    , subtractFrom1
    , sum
    , times
    , unPosProperRational
    , half, third, third2, quarter, quarter3, eighth, eighth3, eighth5, eighth7, tenth, twelfth
    )
    where

import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromJust)
import           GHC.Generics (Generic)

import           PlutusTx.Prelude
                    ( otherwise,
                    (>>=),
                    Integer,
                    Maybe(..),
                    Eq,
                    Ord((<)),
                    ($),
                    (.),
                    AdditiveGroup((-)) )
import qualified PlutusTx
import qualified Prelude as H--askell   
import           Schema (ToSchema) 

import qualified Lib.NaturalNumber.Extra as E
import qualified Lib.NaturalNumber.NatGE1 as N1 (mkOk, nat1)
import           Lib.NaturalNumber.NatGE1 (NatGE1)
import qualified Lib.PosRational as R  
import           Lib.PosRational (PosRational)


newtype PosProperRational =  PosProperRational {unPosProperRational :: PosRational} 
    deriving stock (H.Eq, H.Ord, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''PosProperRational 


mkPosProperRational :: PosRational -> Maybe PosProperRational
mkPosProperRational r 
    | R.numerator r < R.denominator r = Just $ PosProperRational r
    | otherwise = Nothing


{-# INLINABLE mkOk #-}
mkOk :: PosRational -> PosProperRational
mkOk = fromJust . mkPosProperRational 


{-# INLINABLE numerator #-}
numerator :: PosProperRational -> Integer
numerator = R.numerator . unPosProperRational


{-# INLINABLE denominator #-}
denominator :: PosProperRational -> Integer
denominator = R.denominator . unPosProperRational


{-# INLINABLE plus #-}
plus :: PosProperRational -> PosProperRational -> PosRational
plus (PosProperRational x) (PosProperRational y) = x `R.plus` y


{-# INLINABLE minus #-}
minus :: PosProperRational -> PosProperRational -> Maybe PosProperRational
minus (PosProperRational x) (PosProperRational y) = x `R.minus` y >>= mkPosProperRational


{-# INLINABLE times #-}
times :: PosProperRational -> PosProperRational -> PosProperRational
times (PosProperRational x) (PosProperRational y) = mkOk $ x `R.times` y


{-# INLINABLE posTimes #-}
posTimes :: NatGE1 -> PosProperRational -> NatGE1
posTimes x = R.posTimes x . unPosProperRational


sum :: NE.NonEmpty PosProperRational -> PosRational
sum = R.sum . NE.map unPosProperRational 


subtractFrom1 :: PosProperRational -> PosProperRational
subtractFrom1 r = mkOk $ R.mkPosRational (N1.mkOk numer) (N1.mkOk denom)
    where 
        numer = denom - numerator r
        denom = denominator r


half, third, third2, quarter, quarter3, eighth, eighth3, eighth5, eighth7, tenth, twelfth :: PosProperRational
half     = mkOk R.half
third    = mkOk R.third
third2   = mkOk R.third2
quarter  = mkOk R.quarter
quarter3 = mkOk R.quarter3
eighth   = mkOk R.eighth
eighth3  = mkOk R.eighth3
eighth5  = mkOk R.eighth5
eighth7  = mkOk R.eighth7    
tenth    = mkOk R.tenth
twelfth  = mkOk R.twelfth