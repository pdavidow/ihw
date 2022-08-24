{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Equity 
    ( Equity(..)
    , fraction
    , fromValue
    , singleton
    , total
    , totalTx
    , toValue 
    )
    where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Function ((&)) 
import qualified Data.List.NonEmpty as NE
import           GHC.Generics (Generic)

import qualified PlutusTx.AssocMap as Map
import qualified Plutus.V1.Ledger.Value as V
import           Plutus.V1.Ledger.Value (AssetClass, Value, assetClassValue, split)
import           PlutusTx.Prelude
                    ( otherwise,
                    Maybe(..),
                    Eq,
                    Monoid(..),
                    Semigroup(..),
                    ($),
                    flip,
                    foldr,
                    (<$>),
                    map )
import           PlutusTx ( FromData, ToData, UnsafeFromData, makeLift ) 
import qualified Prelude as H--askell   
import           Schema (ToSchema)   

import           Lib.NaturalNumber.NatGE1 as N1 ( mkOk, plus, NatGE1(..) )
import           Lib.PosProperRational as PR ( posTimes, PosProperRational )
    

-- wasteful structure since long CurrencySymbol is unneccesarily repetitive
-- Alos, should use NEPMAP, and forget about mempty
newtype Equity = Equity {unEquity :: Map.Map AssetClass NatGE1} 
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

makeLift ''Equity


singleton :: V.AssetClass -> NatGE1 -> Equity
singleton k v = Equity $ Map.singleton k v


{-# INLINABLE fraction #-}
fraction :: Equity -> PosProperRational -> Equity
fraction tot r = Equity $ flip PR.posTimes r <$> unEquity tot


toValue :: Equity -> Value
toValue e = 
    unEquity e
        & Map.toList
        & map (\(k, v) -> assetClassValue k $ unNatGE1 v)
        & foldr (<>) mempty


fromValue :: V.Value -> Maybe Equity  
fromValue v 
    | V.isZero neg = Just $ Equity m
    | otherwise = Nothing
    where 
        (neg, pos) = split v  
        f = \(c,t,i) -> (V.assetClass c t, N1.mkOk i)
        m = Map.fromList $ map f $ V.flattenValue pos


total :: NE.NonEmpty Equity -> Equity
total x = 
    x    
        & NE.map unEquity
        & H.foldr1 (Map.unionWith N1.plus)
        & Equity


-- total' :: NE.NonEmpty Equity -> Equity
-- total' = totalTx . NE.toList 

 
{-# INLINABLE totalTx #-}
totalTx :: [Equity] -> Equity
totalTx x = 
    x    
        & map unEquity        
        & foldr (Map.unionWith N1.plus) Map.empty
        & Equity        

        
-------------------------------------------
instance H.Semigroup Equity where
    e1 <> e2 = total $ NE.fromList [e1, e2]

instance Semigroup Equity where
    e1 <> e2 = totalTx [e1, e2]

instance H.Monoid Equity where
    mempty = Equity Map.empty

instance Monoid Equity where
    mempty = Equity Map.empty
-------------------------------------------    