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

module Auction.BidderStatus
    ( approveBidders
    , isBidderApproved
    , isBidderRegistered
    , registerBidder
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

import qualified Auction.CertApprovals as CA
import qualified Auction.CertRegistration as CR

import           Auction.Status
import           Auction.Synonyms


isBidderRegistered :: BiddersMap -> PubKeyHash -> Bool 
isBidderRegistered m x = maybe False (== Registered) $ AssocMap.lookup x m


isBidderApproved :: BiddersMap -> PubKeyHash -> Bool 
isBidderApproved m x = maybe False (== Approved) $ AssocMap.lookup x m


registerBidder :: BiddersMap -> CR.CertRegistration -> BiddersMap
registerBidder m x = AssocMap.insert (CR.pkhFor x) Registered m


approveBidders :: BiddersMap -> CA.CertApprovals -> BiddersMap
approveBidders m x = foldr (`AssocMap.insert` Approved) m $ CA.pkhsFor x


