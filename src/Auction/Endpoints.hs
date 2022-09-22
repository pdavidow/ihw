{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Auction.Endpoints
    ( AuctionSchema
    , endpoints
    )
    where
  
import           Control.Monad ( void ) 
import           Data.Monoid (Last (..))
import qualified Data.Text as T

import           Ledger ( PubKeyHash )

import           Plutus.Contract
                    ( Contract,
                    type (.\/),
                    Endpoint,
                    Promise(awaitPromise),
                    endpoint,
                    ownPubKeyHash,
                    mapError,
                    select,
                    logInfo,
                    tell )
import           Plutus.Contract.StateMachine ( SMContractError, getThreadToken, runInitialise, runStep )

import           Auction.Bidders ( mkBidders ) 
import           Auction.Chain ( auctionClient ) 
import           Auction.Share ( auctionedTokenValue )
import           Auction.Types ( Seller(..), Bid(..), AuctionParams(..), AuctionDatum(..), AuctionRedeemer(..), StartParams(..) ) 


type AuctionSchema =
        Endpoint "start"    StartParams
    .\/ Endpoint "bid"      (AuctionParams, Integer)
    .\/ Endpoint "close"    AuctionParams
    .\/ Endpoint "register" AuctionParams
    .\/ Endpoint "approve"  (AuctionParams, [PubKeyHash])


endpoints :: Contract (Last AuctionParams) AuctionSchema T.Text ()
endpoints = awaitPromise 
    ( start'    `select` 
      bid'      `select` 
      close'    `select` 
      register' `select` 
      approve'
    ) >> endpoints
  where
    start'    = endpoint @"start"    start
    bid'      = endpoint @"bid"      bid
    close'    = endpoint @"close"    close
    register' = endpoint @"register" register
    approve'  = endpoint @"approve"  approve


mapErrorSM :: Contract w s SMContractError a -> Contract w s T.Text a
mapErrorSM = mapError $ T.pack . show


start :: StartParams -> Contract (Last AuctionParams) AuctionSchema T.Text ()
start StartParams{..} = do         
    pkh <- ownPubKeyHash         
    threadToken <- mapErrorSM getThreadToken

    let params =             
            AuctionParams
                { apSeller = Seller pkh
                , apDeadline = spDeadline
                , apMinBid = spMinBid
                , apAsset = spAsset
                , apAnchor = threadToken 
                } 

    let client = auctionClient params
    let datum = InProgress Nothing mkBidders
    let val = auctionedTokenValue (apAsset params)
       
    void $ mapErrorSM $ runInitialise client datum val
    tell $ Last $ Just params

    logInfo $ "started auction: " ++ show params

 
register :: AuctionParams -> Contract w AuctionSchema T.Text ()
register params = do
    self <- ownPubKeyHash
    void $ mapErrorSM $ runStep (auctionClient params) $ Register self


approve :: (AuctionParams, [PubKeyHash]) -> Contract w AuctionSchema T.Text ()
approve (params, pkhs) = do
    self <- ownPubKeyHash
    void $ mapErrorSM $ runStep (auctionClient params) $ Approve self pkhs


bid :: (AuctionParams, Integer) -> Contract w AuctionSchema T.Text ()
bid (params, n) = do 
    self <- ownPubKeyHash
    void $ mapErrorSM $ runStep (auctionClient params) $ MkBid $ Bid self n


close :: AuctionParams -> Contract w AuctionSchema T.Text ()
close params = void $ mapErrorSM $ runStep (auctionClient params) Close