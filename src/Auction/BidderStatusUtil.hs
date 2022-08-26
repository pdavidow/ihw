{-# LANGUAGE NoImplicitPrelude #-}

module Auction.BidderStatusUtil
    ( isBidderApproved
    , isBidderRegistered
    ) 
    where

import           Ledger ( PubKeyHash ) 

import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude ( Bool, Eq((==)), Maybe(..) ) 

import           Auction.Status ( Status(Approved, Registered) )
import           Auction.Synonyms ( BiddersMap )


{-# INLINABLE isBidderRegistered #-}
isBidderRegistered :: BiddersMap -> PubKeyHash -> Bool 
isBidderRegistered m x = Just Registered == AssocMap.lookup x m


{-# INLINABLE isBidderApproved #-}
isBidderApproved :: BiddersMap -> PubKeyHash -> Bool 
isBidderApproved m x = Just Approved == AssocMap.lookup x m
