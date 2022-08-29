{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Auction.AuctionSchema
    ( AuctionSchema
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
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import           Ledger.Value ( assetClassValueOf ) 
import qualified Plutus.Contracts.Currency as Currency

import           Anchor ( anchorAsset, anchorTokenName, anchorValue, AnchorGraveyard(..), Anchor(Anchor) )
import           Auction.BidderStatus ( registerBidder, approveBidders ) 
import           Auction.BidderStatusUtil ( isBidderApproved )
import qualified Auction.Approvals as CA
import qualified Auction.Registration as CR
import           Auction.TypesNonCertBidderStatus ( NotRegistereds(..), AlreadyApproveds(..) )
import           Auction.Onchain ( auctionAddress, auctionValidator, typedAuctionValidator, typedValidator )                   
import           Auction.Share ( auctionDatum, minBid, minLovelace, auctionedTokenValue )
import           Auction.Types ( Auction(..), Bid(..), AuctionAction(..), AuctionDatum(..), CloseParams(..), BidParams(..), ApproveParams(..), RegisterParams(..), StartParams(..) )


type AuctionSchema =
        Endpoint "start"    StartParams
    .\/ Endpoint "bid"      BidParams
    .\/ Endpoint "close"    CloseParams
    .\/ Endpoint "register" RegisterParams
    .\/ Endpoint "approve"  ApproveParams

