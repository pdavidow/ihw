{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Auction.Status
    ( Status(..)
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import qualified PlutusTx
import           PlutusTx.Prelude ( Bool(False, True), Eq(..) ) 
import qualified Prelude as P   
import           Schema (ToSchema)


data Status = Registered | Approved
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq Status where
    {-# INLINABLE (==) #-}
    Registered == Registered = True
    Approved == Approved = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''Status [('Registered, 0), ('Approved, 1)]
PlutusTx.makeLift ''Status  