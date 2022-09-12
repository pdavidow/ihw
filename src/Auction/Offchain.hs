{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Auction.Offchain
    ( AuctionSchema
    , endpoints
    )
    where
  
import           Control.Monad ( unless, when, void ) 
import qualified Data.Map as Map
import           Data.Monoid (Last (..))
import qualified Data.Text as T
import           Text.Printf (printf)

import           Ledger
                    ( toTxOut,
                    from,
                    to,
                    getCardanoTxId,
                    TxOut(txOutValue),
                    Redeemer(Redeemer),
                    ChainIndexTxOut,
                    TxOutRef )
import           Ledger.Ada as Ada ( lovelaceValueOf )
import qualified Ledger.Constraints as Constraints
import           Plutus.ChainIndex.Tx ( ChainIndexTx(_citxData) )
import           Plutus.Contract
                    ( utxosTxOutTxAt,
                    mapError,
                    logError,
                    submitTxConstraintsWith,
                    logInfo,
                    tell,
                    awaitTxConfirmed,
                    submitTxConstraints,
                    ownPubKeyHash,
                    endpoint,
                    select,
                    type (.\/),
                    Endpoint,
                    throwError,
                    Promise(awaitPromise),
                    Contract )
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           Ledger.Value ( assetClassValueOf ) 
import qualified Plutus.Contracts.Currency as Currency

import           Auction.Bidders ( NotRegistereds(..), AlreadyApproveds(..), pkhsForApprovals, mkBidders, isBidderApproved, registerBidder, approveBidders, validateRegisteree, validateApprovees )
import           Auction.Onchain ( auctionAddress, auctionValidator, typedAuctionValidator, typedValidator )                   
import           Auction.Share ( auctionDatum, minBid, minLovelace, auctionedTokenValue )
import           Auction.Types ( Auction(..), Bid(..), AuctionRedeemer(..), AuctionDatum(..), BidParams(..), ApproveParams(..), Seller(..), StartParams(..) )


type AuctionSchema =
        Endpoint "start"    StartParams
    .\/ Endpoint "bid"      BidParams
    .\/ Endpoint "close"    
    .\/ Endpoint "register" 
    .\/ Endpoint "approve"  ApproveParams


endpoints :: Contract (Last ThreadToken) AuctionSchema T.Text ()
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


mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show


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
    let val = auctionedTokenValue (apAsset params) <> Ada.lovelaceValueOf minLovelace
       
    void $ mapErrorSM $ runInitialise client datum val
    tell $ Last $ Just params

    logInfo $ "started auction: " ++ show params

 
register :: AuctionParams -> Contract w AuctionSchema T.Text ()
register params = do
    self <- ownPubKeyHash
    void $ mapErrorSM $ runStep (auctionClient params) $ Register self


approve :: AuctionParams -> [PubKeyHash]  -> Contract w AuctionSchema T.Text ()
approve params pkhs = do
    self <- ownPubKeyHash
    void $ mapErrorSM $ runStep (auctionClient params) $ Approve self pkhs


bid :: AuctionParams -> Integer -> Contract w AuctionSchema T.Text ()
bid params n = do 
    self <- ownPubKeyHash
    void $ mapErrorSM $ runStep (auctionClient params) $ MkBid $ Bid self n


close :: AuctionParams -> Contract w AuctionSchema T.Text ()
cose params = mapErrorSM $ runStep (auctionClient params) Close