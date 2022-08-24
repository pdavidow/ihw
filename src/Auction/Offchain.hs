{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Auction.Offchain
    ( AuctionSchema
    , endpoints
    )
    where
  
import           Control.Monad 
import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe ) 
import           Data.Monoid (Last (..))
import qualified Data.Text as T
import           Text.Printf (printf)

import           Ledger
import           Ledger.Ada           as Ada
import qualified Ledger.Constraints as Constraints

import           Plutus.ChainIndex.Tx ( ChainIndexTx(_citxData) )
import           Plutus.Contract
import qualified PlutusTx
import           Ledger.Value ( assetClassValue, assetClassValueOf ) 
import qualified Plutus.Contracts.Currency as Currency

import           Anchor
import           Auction.Onchain                   
import           Auction.Share


type AuctionSchema =
        Endpoint "start" StartParams
    .\/ Endpoint "bid"   BidParams
    .\/ Endpoint "close" CloseParams


endpoints :: Contract (Last Anchor) AuctionSchema T.Text ()
endpoints = awaitPromise (start' `select` bid' `select` close') >> endpoints
  where
    start' = endpoint @"start" start
    bid'   = endpoint @"bid"   bid
    close' = endpoint @"close" close


start :: StartParams -> Contract (Last Anchor) AuctionSchema T.Text ()
start StartParams{..} = do         
    pkh <- ownPubKeyHash
    anchor <- mintAnchor           
 
    let a = Auction
            { aSeller   = pkh
            , aDeadline = spDeadline
            , aMinBid   = spMinBid
            , aCurrency = spCurrency
            , aToken    = spToken
            }
 
    let d = AuctionDatum
            { adAuction    = a
            , adHighestBid = Nothing
            }
 
    -- let v = mempty 
    -- let v = anchorValue anchor <> Ada.lovelaceValueOf minLovelace
    let v = anchorValue anchor <> auctionedTokenValue a <> Ada.lovelaceValueOf minLovelace

    let tx = Constraints.mustPayToTheScript (PlutusTx.toBuiltinData d) v

    ledgerTx <- submitTxConstraints typedValidator tx       
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    tell $ Last $ Just anchor -- broadcasted only after tx confirmed

    logInfo @String $ printf "started auction %s for value-with-token %s" (show a) (show v)


bid :: BidParams -> Contract w AuctionSchema T.Text ()
bid BidParams{..} = do 
    mbX <- findViaAnchor bpAnchor
    (oref, o, d@AuctionDatum{..}) <- case mbX of
        Nothing -> throwError "anchor not found" 
        Just x -> pure x
    logInfo @String $ printf "found auction utxo with datum %s" $ show d        

    when (bpBid < minBid d) $
        throwError $ T.pack $ printf "bid lower than minimal bid %d" $ minBid d

    pkh <- ownPubKeyHash

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

    void $ awaitTime $ aDeadline adAuction             

    let t      = auctionedTokenValue adAuction
        r      = Redeemer $ PlutusTx.toBuiltinData Close
        seller = aSeller adAuction

        lookups = Constraints.typedValidatorLookups typedAuctionValidator <>
                  Constraints.otherScript auctionValidator                <>
                  Constraints.unspentOutputs (Map.singleton oref o)

        tx      = case adHighestBid of
                    Nothing      -> Constraints.mustPayToPubKey seller (t <> Ada.lovelaceValueOf minLovelace)  <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r

                    Just Bid{..} -> Constraints.mustPayToPubKey bBidder (t <> Ada.lovelaceValueOf minLovelace) <>
                                    Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf bBid)              <>
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

    logInfo @String "ended buryAnchor"