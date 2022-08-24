{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Auction.ValidateBid
    ( validateBid
    , validateBidMeetsIncrement
    , validateBidMeetsReserve
    )
    where

import           PlutusTx.Prelude

import           Auction.Types
import           Auction.Utility ( isBiddingAlreadyStarted ) 


{-# INLINABLE validateBid #-}
validateBid :: AuctionDatum -> Submit -> Maybe BidError
validateBid x y = 
    if isBiddingAlreadyStarted x 
        then validateBidMeetsIncrement x y
        else validateBidMeetsReserve x y 


{-# INLINABLE validateBidMeetsReserve #-}
validateBidMeetsReserve :: AuctionDatum -> Submit -> Maybe BidError
validateBidMeetsReserve AuctionDatum{..} (newBid, _) = 
    if newBid >= adReservePrice then 
        Nothing
    else 
        Just BelowReserve 


{-# INLINABLE validateBidMeetsIncrement #-}
validateBidMeetsIncrement :: AuctionDatum -> Submit -> Maybe BidError
validateBidMeetsIncrement AuctionDatum{..} (newBid, _) = do
    let oldHighestSubmit = adHighestSubmit

    (oldBid, _) <- oldHighestSubmit 
    if newBid >= oldBid + adBidIncrement then 
        Nothing
    else 
        Just BelowIncrement 