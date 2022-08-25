{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Auction.CertRegistration
    ( CertRegistration -- hide constructor
    , certifyRegisteree
    , pkhFor
    ) 
    where


import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Data.Text as T

import           Ledger 
import           Ledger.Value as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude
import qualified Prelude as P   
import           Schema (ToSchema)

import           Auction.BidderStatusUtil
import           Auction.Synonyms


newtype CertRegistration = CertRegistration PubKeyHash 
    deriving stock (P.Eq, P.Ord, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''CertRegistration


certifyRegisteree :: BiddersMap -> PubKeyHash -> Either T.Text CertRegistration
certifyRegisteree m x
  | isBidderRegistered m x = Left "already registered"
  | isBidderApproved m x = Left "already approved"
  | otherwise = Right $ CertRegistration x


{-# INLINABLE pkhFor #-}
pkhFor :: CertRegistration -> PubKeyHash
pkhFor (CertRegistration x) = x