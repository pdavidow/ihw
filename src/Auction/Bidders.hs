{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Auction.Bidders 
    ( Bidders -- hide constructor
    )
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Ledger ( PubKeyHash ) 
import qualified PlutusTx
import           PlutusTx.Prelude ( Eq )
import qualified PlutusTx.AssocMap as AssocMap
import qualified Prelude as P   
import           Schema (ToSchema)

import           Auction.Status ( Status )


newtype Bidders = Bidders (AssocMap.Map PubKeyHash Status)
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''Bidders