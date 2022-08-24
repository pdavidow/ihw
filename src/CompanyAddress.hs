{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CompanyAddress     
    ( CompanyAddress -- hide constructor
    , testCompanyAddress
    , unCompanyAddress
    )
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import           Ledger (PubKeyHash)
import qualified PlutusTx
import           PlutusTx.Prelude (Eq(..))
import qualified Prelude as Haskell  
import           Schema (ToSchema)

import           Utility (companyPkh)


newtype CompanyAddress = CompanyAddress {unCompanyAddress :: PubKeyHash}
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq) 

PlutusTx.makeIsDataIndexed ''CompanyAddress [('CompanyAddress, 0)]
PlutusTx.makeLift ''CompanyAddress  


testCompanyAddress :: CompanyAddress
testCompanyAddress = CompanyAddress companyPkh