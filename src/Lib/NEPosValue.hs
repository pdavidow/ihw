{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- NE == Not Empty

module Lib.NEPosValue 
    ( NEPosValue -- hiding constructor
    , assetClassValueOf
    , flattenValue
    , fraction
    , mkNEPosValue
    , mkOk    
    , singleton
    , toValue
    , unNEPosValue
    , valueOf   
    ) where

import           Codec.Serialise.Class            (Serialise)
import           Control.DeepSeq                  (NFData)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Maybe (fromJust)
import           GHC.Generics (Generic)

import qualified Plutus.V1.Ledger.Value as V
import           PlutusTx.Prelude
                    ( otherwise,
                    Maybe(..),
                    Eq,
                    Ord((>)),
                    ($),
                    (.),
                    length,
                    Semigroup(..) )             
                   
import           PlutusTx ( FromData, ToData, UnsafeFromData, makeLift ) 
import qualified Prelude as H--askell   
import           Schema (ToSchema)   

import           Lib.NaturalNumber.NatGE1 as N1 (nat1, NatGE1(..))
import           Lib.PosProperRational as PR ( PosProperRational )
import qualified Lib.PosValue as PV
import           Lib.PosValue (PosValue)


newtype NEPosValue = NEPosValue {unNEPosValue :: PosValue} 
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData, ToSchema)
    deriving newtype (Eq, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

makeLift ''NEPosValue

-------------------------------------------
instance H.Semigroup NEPosValue where
    x <> y = NEPosValue $ unNEPosValue x <> unNEPosValue y

instance Semigroup NEPosValue where
    x <> y = NEPosValue $ unNEPosValue x <> unNEPosValue y
-------------------------------------------  


mkNEPosValue :: PosValue -> Maybe NEPosValue
mkNEPosValue pv
    | length (PV.flattenValue pv) > 0 = Just $ NEPosValue pv
    | otherwise = Nothing  


{-# INLINABLE singleton #-}
singleton :: V.CurrencySymbol -> V.TokenName -> NatGE1 -> NEPosValue
singleton c t nat = NEPosValue $ PV.singleton c t nat


{-# INLINABLE mkOk #-}
mkOk :: PosValue -> NEPosValue
mkOk = fromJust . mkNEPosValue 


{-# INLINABLE valueOf #-}
valueOf :: NEPosValue -> V.CurrencySymbol -> V.TokenName -> NatGE1
valueOf x = PV.valueOf (unNEPosValue x)


{-# INLINABLE assetClassValueOf #-}
assetClassValueOf :: NEPosValue -> V.AssetClass -> NatGE1
assetClassValueOf x = PV.assetClassValueOf (unNEPosValue x) 


{-# INLINABLE flattenValue #-}
flattenValue :: NEPosValue -> [(V.CurrencySymbol, V.TokenName, NatGE1)]
flattenValue x = PV.flattenValue (unNEPosValue x)


{-# INLINABLE fraction #-}
fraction :: NEPosValue -> PosProperRational -> NEPosValue
fraction x r = NEPosValue $ PV.fraction (unNEPosValue x) r   


{-# INLINABLE toValue #-}
toValue :: NEPosValue -> V.Value
toValue = PV.unPosValue . unNEPosValue