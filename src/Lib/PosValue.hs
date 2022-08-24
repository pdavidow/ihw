{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib.PosValue
    ( PosValue -- hiding constructor
    , assetClassValueOf
    , flattenValue
    , fraction
    , mkOk
    , mkPosValue
    , singleton
    , unPosValue
    , valueOf
    )
    where

import           Codec.Serialise.Class            (Serialise)
import           Control.DeepSeq                  (NFData)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Maybe (fromJust)
import           GHC.Generics (Generic)

import qualified Plutus.V1.Ledger.Value as V
import           Plutus.V1.Ledger.Value (Value)
import qualified PlutusTx.AssocMap as Map
import           PlutusTx.Prelude
                    ( otherwise,
                    return,
                    Integer,
                    Maybe(..),
                    Eq,
                    Ord((>)),
                    ($),
                    (.),
                    all,
                    foldr,
                    (<$>),
                    Monoid(..),
                    Semigroup(..) )                  
                   
import           PlutusTx ( FromData, ToData, UnsafeFromData, makeLift ) 
import qualified Prelude as H--askell   
import           Schema (ToSchema)   

import           Lib.NaturalNumber.NatGE1 as N1 (nat1, NatGE1(..))
import qualified Lib.NaturalNumber.NatGE1 as N1 (mkOk)
import           Lib.PosProperRational as PR ( posTimes, PosProperRational )


newtype PosValue = PosValue {unPosValue :: Value} 
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, NFData, ToSchema)
    deriving newtype (Eq, Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

makeLift ''PosValue

-------------------------------------------
instance H.Semigroup PosValue where
    x <> y = PosValue $ unPosValue x <> unPosValue y

instance Semigroup PosValue where
    x <> y = PosValue $ unPosValue x <> unPosValue y

instance H.Monoid PosValue where
    mempty = PosValue H.mempty

instance Monoid PosValue where
    mempty = PosValue mempty
-------------------------------------------  


flattenValueKeep0 :: Value -> [(V.CurrencySymbol, V.TokenName, Integer)]
flattenValueKeep0 v = do
    (cs, m) <- Map.toList $ V.getValue v
    (tn, a) <- Map.toList m
    -- guard $ a /= 0
    return (cs, tn, a)


{-# INLINABLE mkPosValue #-}
mkPosValue :: Value -> Maybe PosValue
mkPosValue v 
    | all (\(_, _, n) -> n > 0) (flattenValueKeep0 v) = Just $ PosValue v
    | otherwise = Nothing       


{-# INLINABLE singleton #-}
singleton :: V.CurrencySymbol -> V.TokenName -> NatGE1 -> PosValue
singleton c t nat = PosValue $ V.singleton c t $ unNatGE1 nat


{-# INLINABLE mkOk #-}
mkOk :: Value -> PosValue
mkOk = fromJust . mkPosValue 


{-# INLINABLE valueOf #-}
valueOf :: PosValue -> V.CurrencySymbol -> V.TokenName -> NatGE1
valueOf x c t = N1.mkOk $ V.valueOf (unPosValue x) c t


{-# INLINABLE assetClassValueOf #-}
assetClassValueOf :: PosValue -> V.AssetClass -> NatGE1
assetClassValueOf x y = N1.mkOk $ V.assetClassValueOf (unPosValue x) y


{-# INLINABLE flattenValue #-}
flattenValue :: PosValue -> [(V.CurrencySymbol, V.TokenName, NatGE1)]
flattenValue x = f <$> V.flattenValue (unPosValue x)
    where f (c, t, n) = (c, t, N1.mkOk n)


{-# INLINABLE fraction #-}
fraction :: PosValue -> PosProperRational -> PosValue
fraction x r = foldr f mempty $ flattenValue x
    where 
        f :: (V.CurrencySymbol, V.TokenName, NatGE1) -> PosValue -> PosValue
        f (c, t, nat) acc = singleton c t (posTimes nat r) <> acc
