{-# LANGUAGE NoImplicitPrelude #-}

module Auction.BidderStatus
    ( approveBidders
    , isBidderApproved
    , isBidderRegistered
    , registerBidder
    ) 
    where

import           Ledger ( PubKeyHash ) 
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude ( Bool, Eq((==)), ($), foldr, Maybe(..) ) 

import qualified Auction.CertApprovals as CA
import qualified Auction.CertRegistration as CR
import           Auction.Status ( Status(..) )
import           Auction.Synonyms ( BiddersMap )


isBidderRegistered :: BiddersMap -> PubKeyHash -> Bool 
isBidderRegistered m x = Just Registered == AssocMap.lookup x m

isBidderApproved :: BiddersMap -> PubKeyHash -> Bool 
isBidderApproved m x = Just Approved == AssocMap.lookup x m


registerBidder :: BiddersMap -> CR.CertRegistration -> BiddersMap
registerBidder m x = AssocMap.insert (CR.pkhFor x) Registered m


approveBidders :: BiddersMap -> CA.CertApprovals -> BiddersMap
approveBidders m x = foldr (`AssocMap.insert` Approved) m $ CA.pkhsFor x


