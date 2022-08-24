{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings  #-}

module CompanyFee
    ( CompanyFee(..)
    , CompanyFee.applyAsCeiling
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)


import qualified PlutusTx
import           PlutusTx.Prelude ( (.), Eq, Ord ) 
import qualified Prelude as H--askell   
import           Schema (ToSchema)   

import           Lib.NaturalNumber.NatGE0 as N0 ( NatGE0 )
import           Lib.NaturalNumber.NatGE1 as N1 ( NatGE1 )
import           PercentTimesTen ( PercentTimesTen, applyAsCeiling )


newtype CompanyFee = CompanyFee {unCompanyFee :: PercentTimesTen}
    deriving stock (H.Eq, H.Ord, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)    
    deriving newtype (Eq, Ord)

PlutusTx.makeIsDataIndexed ''CompanyFee [('CompanyFee, 0)]
PlutusTx.makeLift ''CompanyFee 


{-# INLINABLE applyAsCeiling #-}
applyAsCeiling :: NatGE1 -> CompanyFee -> NatGE0
applyAsCeiling total = PercentTimesTen.applyAsCeiling total . unCompanyFee