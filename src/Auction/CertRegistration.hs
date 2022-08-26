{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Auction.CertRegistration
    ( CertRegistration -- hide constructor
    , certifyRegisteree
    , pkhFor
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import qualified Data.Text as T

import           Ledger ( PubKeyHash(PubKeyHash) ) 

import qualified PlutusTx

import           PlutusTx.Prelude ( otherwise, Either(..), Eq, Ord, ($) )
import qualified Prelude as P   
import           Schema (ToSchema)

import           Auction.BidderStatusUtil ( isBidderRegistered, isBidderApproved )
import           Auction.Synonyms ( BiddersMap )


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