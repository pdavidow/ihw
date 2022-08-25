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

module Auction.CertApprovals 
    ( CertApprovals -- hide constructor
    , certifyApprovees
    , pkhsFor
    )
    where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           Ledger 
import           Ledger.Value as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude 
import qualified Prelude as P   
import           Schema (ToSchema)

import           Auction.BidderStatusUtil
import           Auction.Synonyms ( BiddersMap )
import           Auction.TypesNonCertBidderStatus


newtype CertApprovals = CertApprovals [PubKeyHash] 
    deriving stock (P.Eq, P.Ord, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''CertApprovals


certifyApprovees :: BiddersMap -> [PubKeyHash] -> (CertApprovals, AlreadyApproveds, NotRegistereds)
certifyApprovees m = foldr f (CertApprovals [], AlreadyApproveds [], NotRegistereds [])
    where f = \ x (CertApprovals as, AlreadyApproveds bs, NotRegistereds cs) ->
            if isBidderRegistered m x then    (CertApprovals $ x:as, AlreadyApproveds bs    , NotRegistereds cs    )
            else if isBidderApproved m x then (CertApprovals as    , AlreadyApproveds $ x:bs, NotRegistereds cs    )
            else                              (CertApprovals as    , AlreadyApproveds bs    , NotRegistereds $ x:cs)


{-# INLINABLE pkhsFor #-}
pkhsFor :: CertApprovals -> [PubKeyHash]
pkhsFor (CertApprovals xs) = xs