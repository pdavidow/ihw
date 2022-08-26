{-# LANGUAGE NoImplicitPrelude #-}

module Auction.BidderStatus
    ( approveBidders
    , registerBidder
    ) 
    where

import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude ( ($), foldr ) 

import qualified Auction.CertApprovals as CA
import qualified Auction.CertRegistration as CR
import           Auction.Status ( Status(..) )
import           Auction.Synonyms ( BiddersMap )


registerBidder :: BiddersMap -> CR.CertRegistration -> BiddersMap
registerBidder m x = AssocMap.insert (CR.pkhFor x) Registered m


approveBidders :: BiddersMap -> CA.CertApprovals -> BiddersMap
approveBidders m x = foldr (`AssocMap.insert` Approved) m $ CA.pkhsFor x