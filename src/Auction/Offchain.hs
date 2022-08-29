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
import qualified PlutusTx
import           Ledger.Value ( assetClassValueOf ) 
import qualified Plutus.Contracts.Currency as Currency

import           Anchor ( anchorAsset, anchorTokenName, anchorValue, AnchorGraveyard(..), Anchor(Anchor) )
import           Auction.Bidders ( NotRegistereds(..), AlreadyApproveds(..), pkhsForApprovals, mkBidders, isBidderApproved, registerBidder, approveBidders, validateRegisteree, validateApprovees )
import           Auction.Onchain ( auctionAddress, auctionValidator, typedAuctionValidator, typedValidator )                   
import           Auction.Share ( auctionDatum, minBid, minLovelace, auctionedTokenValue )
import           Auction.Types ( Auction(..), Bid(..), AuctionAction(..), AuctionDatum(..), CloseParams(..), BidParams(..), ApproveParams(..), RegisterParams(..), Seller(..), StartParams(..) )


type AuctionSchema =
        Endpoint "start"    StartParams
    .\/ Endpoint "bid"      BidParams
    .\/ Endpoint "close"    CloseParams
    .\/ Endpoint "register" RegisterParams
    .\/ Endpoint "approve"  ApproveParams


endpoints :: Contract (Last Anchor) AuctionSchema T.Text ()
endpoints = awaitPromise (start' `select` bid' `select` close' `select` register' `select` approve') >> endpoints
  where
    start'    = endpoint @"start"    start
    bid'      = endpoint @"bid"      bid
    close'    = endpoint @"close"    close
    register' = endpoint @"register" register
    approve'  = endpoint @"approve"  approve


start :: StartParams -> Contract (Last Anchor) AuctionSchema T.Text ()
start StartParams{..} = do         
    pkh <- ownPubKeyHash
    anchor <- mintAnchor           
 
    let a = Auction
            { aSeller   = Seller pkh
            , aBidders  = mkBidders
            , aDeadline = spDeadline
            , aMinBid   = spMinBid
            , aCurrency = spCurrency
            , aToken    = spToken
            }
 
    let d = AuctionDatum
            { adAuction    = a
            , adHighestBid = Nothing
            , adAnchor     = anchor
            }
 
    let v = anchorValue anchor <> auctionedTokenValue a <> Ada.lovelaceValueOf minLovelace

    let tx = Constraints.mustPayToTheScript (PlutusTx.toBuiltinData d) v

    ledgerTx <- submitTxConstraints typedValidator tx       
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    tell $ Last $ Just anchor -- broadcasted only after tx confirmed

    logInfo @String $ printf "started auction %s for value-with-token %s" (show a) (show v)
   

register :: RegisterParams -> Contract w AuctionSchema T.Text ()
register RegisterParams{..} = do
    mbX <- findViaAnchor rpAnchor
    (oref, o, d@AuctionDatum{..}) <- case mbX of
        Nothing -> throwError "anchor not found" 
        Just x -> pure x
    logInfo @String $ printf "found auction utxo with datum %s" $ show d        

    pkh <- ownPubKeyHash
    when (pkh == unSeller (aSeller adAuction)) $ throwError $ T.pack $ printf "seller may not register" 

    reg <- case validateRegisteree (aBidders adAuction) pkh of
        Left e -> throwError e
        Right x -> pure x

    let d' = d {adAuction = adAuction {aBidders = registerBidder (aBidders adAuction) reg}}
        v  = anchorValue rpAnchor <> auctionedTokenValue adAuction <> Ada.lovelaceValueOf (minLovelace + maybe 0 bBid adHighestBid)
        r  = Redeemer $ PlutusTx.toBuiltinData $ Register reg

        lookups = Constraints.typedValidatorLookups typedAuctionValidator <>
                  Constraints.otherScript auctionValidator                <>
                  Constraints.unspentOutputs (Map.singleton oref o)

        tx      = Constraints.mustPayToTheScript d' v                     <>
                  Constraints.mustSpendScriptOutput oref r

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @String $ printf "registered bidder %s" $ show pkh

 
approve :: ApproveParams -> Contract w AuctionSchema T.Text ()
approve ApproveParams{..} = do
    when (null apApprovals) $ throwError $ T.pack "list may not be empty" 

    mbX <- findViaAnchor apAnchor
    (oref, o, d@AuctionDatum{..}) <- case mbX of
        Nothing -> throwError "anchor not found" 
        Just x -> pure x
    logInfo @String $ printf "found auction utxo with datum %s" $ show d        

    pkh <- ownPubKeyHash
    unless (pkh == unSeller (aSeller adAuction)) $ throwError $ T.pack $ printf "only seller may approve" 

    let (app, AlreadyApproveds alreadyAs, NotRegistereds notRs) = validateApprovees (aBidders adAuction) apApprovals
    let pkhsA = pkhsForApprovals app
    -- when (null pkhsA && length alreadyAs < length apApprovals) $ throwError $ T.pack $ printf "none fit for approval (that haven't already been approved) %s" $ show apApprovals
    when (null pkhsA) $ throwError $ T.pack $ printf "none fit for approval (that haven't already been approved) %s" $ show apApprovals
    unless (null notRs) $ logInfo @String $ printf "not registered %s" $ show notRs
    unless (null alreadyAs) $ logInfo @String $ printf "already approved %s" $ show alreadyAs

    let d' = d {adAuction = adAuction {aBidders = approveBidders (aBidders adAuction) app}}
        v  = anchorValue apAnchor <> auctionedTokenValue adAuction <> Ada.lovelaceValueOf (minLovelace + maybe 0 bBid adHighestBid)
        r  = Redeemer $ PlutusTx.toBuiltinData $ Approve pkh app

        lookups = Constraints.typedValidatorLookups typedAuctionValidator <>
                  Constraints.otherScript auctionValidator                <>
                  Constraints.unspentOutputs (Map.singleton oref o)

        tx      = Constraints.mustPayToTheScript d' v                     <>
                  Constraints.mustSpendScriptOutput oref r

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @String $ printf "approved bidders %s" $ show pkhsA

  
bid :: BidParams -> Contract w AuctionSchema T.Text ()
bid BidParams{..} = do 
    mbX <- findViaAnchor bpAnchor
    (oref, o, d@AuctionDatum{..}) <- case mbX of
        Nothing -> throwError "anchor not found" 
        Just x -> pure x
    logInfo @String $ printf "found auction utxo with datum %s" $ show d        

    when (bpBid < minBid d) $ throwError $ T.pack $ printf "bid lower than minimal bid %d" $ minBid d
  
    pkh <- ownPubKeyHash
    unless (isBidderApproved (aBidders adAuction) pkh) $ throwError $ T.pack $ printf "bidder not approved %s" $ show pkh
 
    let b  = Bid {bBidder = pkh, bBid = bpBid}
        d' = d {adHighestBid = Just b}
        v  = anchorValue bpAnchor <> auctionedTokenValue adAuction <> Ada.lovelaceValueOf (minLovelace + bpBid)
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


close :: CloseParams -> Contract w AuctionSchema T.Text ()
close CloseParams{..} = do       
    mbX <- findViaAnchor cpAnchor
    (oref, o, d@AuctionDatum{..}) <- case mbX of
        Nothing -> do
            let e = "anchor not found" 
            logError e
            throwError e
        Just x -> pure x
    logInfo @String $ printf "found auction utxo with datum %s" (show d)              
 
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
    buryAnchor cpAnchor cpAnchorGraveyard

    logInfo @String $ printf "closed auction %s" $ show adAuction

 
mintAnchor :: Contract w AuctionSchema T.Text Anchor
mintAnchor = do
    pkh <- ownPubKeyHash

    sym <- fmap Currency.currencySymbol $ 
            mapError (T.pack . show @Currency.CurrencyError) $
                Currency.mintContract pkh [(anchorTokenName, 1)]

    pure $ Anchor sym


findViaAnchor :: Anchor -> Contract w s T.Text (Maybe (TxOutRef, ChainIndexTxOut, AuctionDatum))
findViaAnchor anchorSymbol = do
    utxos <- Map.filter f <$> utxosTxOutTxAt auctionAddress
    pure $ case Map.toList utxos of
        [(oref, (o, citx))] -> (oref, o,) <$> auctionDatum (toTxOut o) (\dh -> Map.lookup dh $ _citxData citx)
        _ -> Nothing
    where
        f :: (ChainIndexTxOut, Plutus.ChainIndex.Tx.ChainIndexTx) -> Bool
        f (o, _) = assetClassValueOf (txOutValue $ toTxOut o) (anchorAsset anchorSymbol) == 1                    


buryAnchor :: Anchor -> AnchorGraveyard -> Contract w AuctionSchema T.Text ()
buryAnchor anchor (AnchorGraveyard pkh) = do  
    let lookups = 
            Constraints.typedValidatorLookups typedAuctionValidator <>
            Constraints.otherScript auctionValidator          

    let txC = Constraints.mustPayToPubKey pkh (anchorValue anchor)           
    
    ledgerTx <- submitTxConstraintsWith lookups txC
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx