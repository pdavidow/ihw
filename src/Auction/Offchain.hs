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
    pkh <- ownPubKeyHash
    void $ mapErrorSM $ runStep (auctionClient params) $ Register pkh


approve' :: ApproveParams -> Contract w AuctionSchema T.Text ()
approve' ApproveParams{..} = do
    when (null appApprovals) $ throwError $ T.pack "list may not be empty" 

    -- mbX <- findViaAnchor apAnchor
    -- (oref, o, d@AuctionDatum{..}) <- case mbX of
    --     Nothing -> throwError "anchor not found" 
    --     Just x -> pure x
    -- logInfo @String $ printf "found auction utxo with datum %s" $ show d        

    pkh <- ownPubKeyHash
    unless (pkh == unSeller (aSeller adAuction)) $ throwError $ T.pack $ printf "only seller may approve" 

    let (app, AlreadyApproveds alreadyAs, NotRegistereds notRs) = validateApprovees (aBidders adAuction) apApprovals
    let pkhsA = pkhsForApprovals app   
    when (null pkhsA) $ throwError $ T.pack $ printf "none fit for approval %s" $ show apApprovals
    unless (null notRs) $ logInfo @String $ printf "not registered %s" $ show notRs
    unless (null alreadyAs) $ logInfo @String $ printf "already approved %s" $ show alreadyAs

    let d' = d {adAuction = adAuction {aBidders = approveBidders (aBidders adAuction) app}}
        v  = auctionedTokenValue adAuction <> Ada.lovelaceValueOf (minLovelace + maybe 0 bBid adHighestBid)
        r  = Redeemer $ PlutusTx.toBuiltinData $ Approve pkh app

        lookups = Constraints.typedValidatorLookups typedAuctionValidator <>
                  Constraints.otherScript auctionValidator                <>
                  Constraints.unspentOutputs (Map.singleton oref o)

        tx      = Constraints.mustPayToTheScript d' v                     <>
                  Constraints.mustSpendScriptOutput oref r

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @String $ printf "approved bidders %s" $ show pkhsA


approve :: AuctionParams -> [PubKeyHash]  -> Contract w AuctionSchema T.Text ()
approve params xs = do
    pkh <- ownPubKeyHash
    void $ mapErrorSM $ runStep (auctionClient params) $ Approve pkh xs


bid :: BidParams -> Contract w AuctionSchema T.Text ()
bid BidParams{..} = do 
    -- mbX <- findViaAnchor bpAnchor
    -- (oref, o, d@AuctionDatum{..}) <- case mbX of
    --     Nothing -> throwError "anchor not found" 
    --     Just x -> pure x
    -- logInfo @String $ printf "found auction utxo with datum %s" $ show d        

    when (bpBid < minBid d) $ throwError $ T.pack $ printf "bid lower than minimal bid %d" $ minBid d
  
    pkh <- ownPubKeyHash
    unless (isBidderApproved (aBidders adAuction) pkh) $ throwError $ T.pack $ printf "bidder not approved %s" $ show pkh
 
    let b  = Bid {bBidder = pkh, bBid = bpBid}
        d' = d {adHighestBid = Just b}
        v  = auctionedTokenValue adAuction <> Ada.lovelaceValueOf (minLovelace + bpBid)
        r  = Redeemer $ PlutusTx.toBuiltinData $ MkBid b

        lookups = Constraints.typedValidatorLookups typedAuctionValidator <>
                  Constraints.otherScript auctionValidator                <>
                  Constraints.unspentOutputs (Map.singleton oref o)

        tx      = case adHighestBid of
                    Nothing      -> Constraints.mustPayToTheScript d' v                            <>
                                    Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
                                    Constraints.mustSpendScriptOutput oref r

                    Just Bid{..} -> Constraints.mustPayToTheScript d' v                            <>
                                    Constraints.mustPayToPubKey bBidder (Ada.lovelaceValueOf bBid) <>
                                    Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
                                    Constraints.mustSpendScriptOutput oref r

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @String $ printf "made bid of %d lovelace in auction %s" bpBid (show adAuction)


close :: Contract w AuctionSchema T.Text ()
close = do       
    -- mbX <- findViaAnchor cpAnchor
    -- (oref, o, d@AuctionDatum{..}) <- case mbX of
    --     Nothing -> do
    --         let e = "anchor not found" 
    --         logError e
    --         throwError e
    --     Just x -> pure x
    -- logInfo @String $ printf "found auction utxo with datum %s" (show d)              
 
    let t      = auctionedTokenValue adAuction
        r      = Redeemer $ PlutusTx.toBuiltinData Close
        seller = aSeller adAuction

        lookups = Constraints.typedValidatorLookups typedAuctionValidator <>
                  Constraints.otherScript auctionValidator                <>
                  Constraints.unspentOutputs (Map.singleton oref o)

        tx      = case adHighestBid of
                    Nothing      -> Constraints.mustPayToPubKey (unSeller seller) (t <> Ada.lovelaceValueOf minLovelace)  <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r

                    Just Bid{..} -> Constraints.mustPayToPubKey bBidder (t <> Ada.lovelaceValueOf minLovelace) <>
                                    Constraints.mustPayToPubKey (unSeller seller) (Ada.lovelaceValueOf bBid)              <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @String $ printf "closed auction %s" $ show adAuction

