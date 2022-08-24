{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings  #-}

module PercentTimesTen     
    ( PercentTimesTen -- hide constructor
    , applyAsCeiling
    , PercentTimesTen.isZero
    , PercentTimesTen.mkOk
    , mkPercentTimesTen  
    , unPercentTimesTen
    )
    where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Either ( fromRight )
import           Data.Either.Combinators ( maybeToRight )
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           PlutusTx.Prelude
                    ( Bool,
                    Integer,
                    Either(..),
                    Eq,
                    ($),
                    (.),
                    Ord((>)),
                    Semigroup((<>)) )
import qualified PlutusTx
import qualified Prelude as H--askell   
import           Schema (ToSchema) 

import           Lib.NaturalNumber.NatGE0 as N0 ( NatGE0, isZero, mkNatGE0, nat0 ) 
import           Lib.NaturalNumber.NatGE1 as N1 ( NatGE1, mkOk, unNatGE1 ) 
import           Lib.NaturalNumber.Extra as Extra ( times, ceilDiv ) 


{-# INLINABLE basis #-}
basis :: NatGE1
basis = N1.mkOk 1000 


newtype PercentTimesTen = PercentTimesTen {unPercentTimesTen :: NatGE0}
    deriving stock (H.Eq, H.Ord, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, Ord)

PlutusTx.makeIsDataIndexed ''PercentTimesTen [('PercentTimesTen, 0)]
PlutusTx.makeLift ''PercentTimesTen  


{-# INLINABLE mkPercentTimesTen #-}
mkPercentTimesTen :: Integer -> Either T.Text PercentTimesTen
mkPercentTimesTen n = 
    if n > unNatGE1 basis then 
        Left errorMsg
    else do
        nat <- maybeToRight errorMsg $ N0.mkNatGE0 n
        Right $ PercentTimesTen nat
    where errorMsg = T.pack ("fails: n >= 0 && n <= " <> H.show basis) 


{-# INLINABLE mkOk #-}
mkOk :: Integer -> PercentTimesTen
mkOk = fromRight presumablyNeverDefault . mkPercentTimesTen 
    where presumablyNeverDefault = PercentTimesTen N0.nat0 -- arbitrary


{-# INLINABLE isZero #-}
isZero :: PercentTimesTen -> Bool 
isZero = N0.isZero . unPercentTimesTen


{-# INLINABLE applyAsCeiling #-}
applyAsCeiling :: NatGE1 -> PercentTimesTen -> NatGE0
applyAsCeiling total x = Extra.ceilDiv numerator denominator
    where
        numerator = total `Extra.times` unPercentTimesTen x
        denominator = basis