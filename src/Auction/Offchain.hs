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
import qualified Ledger.Constraints as Constraints

import           Plutus.ChainIndex.Tx ( ChainIndexTx(_citxData) )
import           Plutus.Contract
import qualified PlutusTx
import           Ledger.Value ( assetClassValue, assetClassValueOf ) 
import qualified Plutus.Contracts.Currency as Currency

import           Anchor ( Anchor(..), anchorTokenName, anchorAsset, anchorValue ) 
import           Auction.Onchain
                   
import           Auction.Types
import           Auction.TypesAuctionRedeemer 
import           Auction.Utility ( bidErrorToText, bidVal, toHighestLosing ) 
import           Duration


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
            , aAnchor  = anchor
            }

    let d = AuctionDatum
            { adAuction    = a
            , adHighestBid = Nothing
            }

    let v = anchorValue anchor <> Ada.lovelaceValueOf minLovelace

    let tx = Constraints.mustPayToTheScript (PlutusTx.toBuiltinData d) v

    ledgerTx <- submitTxConstraints typedValidator) tx       
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    tell $ Last $ Just anchor -- broadcasted here, only after tx confirmed

    logInfo @String $ printf "started auction %s for token %s" (show a) (show v)


bid :: BidParams -> Contract w AuctionSchema T.Text ()
bid BidParams{..} = do 
    mbX <- findViaAnchor bpAnchor
    (oref, o, d@AuctionDatum{..}) <- case mbX of
        Nothing -> throwError "anchor not found" 
        Just x -> pure x
    logInfo @String $ printf "found auction utxo with datum %s" (show d)        

    when (bpBid < minBid d) $
        throwError $ pack $ printf "bid lower than minimal bid %d" (minBid d)

    pkh <- ownPubKeyHash

    let b  = Bid {bBidder = pkh, bBid = bpBid}
        d' = d {adHighestBid = Just b}
        v  = anchorValue bpAnchor <> Ada.lovelaceValueOf (minLovelace + bpBid)
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

    logInfo @String $ printf "made bid of %d lovelace in auction %s for token %s"
        bpBid
        (show adAuction)
        (show bpAnchor)


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

    let t      = anchorValue cpAnchor
        r      = Redeemer $ PlutusTx.toBuiltinData Close
        seller = aSeller adAuction

        lookups = Constraints.typedValidatorLookups typedAuctionValidator P.<>
                  Constraints.otherScript auctionValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)

        tx      = case adHighestBid of
                    Nothing      -> Constraints.mustPayToPubKey seller (t <> Ada.lovelaceValueOf minLovelace)  <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r

                    Just Bid{..} -> Constraints.mustPayToPubKey bBidder (t <> Ada.lovelaceValueOf minLovelace) <>
                                    Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf bBid)              <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r
    -- let lookups = 
    --         Constraints.typedValidatorLookups (typedValidator paramCompanyAddress) <>
    --         Constraints.otherScript (mkMyScript paramCompanyAddress) <>            
    --         Constraints.unspentOutputs (Map.singleton oref o)

    -- logInfo @String $ printf "adHighestSubmit %s" $ show adHighestSubmit
    -- let txC = case adHighestSubmit of
    --         Nothing ->           
    --             Constraints.mustValidateIn (from adDeadline) <>              
    --             Constraints.mustSpendScriptOutput oref redeemer <>
    --             Constraints.mustPayToPubKey adSeller adAsset       
    --                 where redeemer = Redeemer $ PlutusTx.toBuiltinData AuctionEndedWinnerNo         
                
    --         Just w@(_, winner) ->
    --             Constraints.mustValidateIn (from adDeadline) <>
    --             Constraints.mustSpendScriptOutput oref redeemer <>
    --             Constraints.mustPayToPubKey adSeller prvNetSellerCredit <>
    --             Constraints.mustPayToPubKey winner (adAsset <> winnerBidCredit) <>
    --             Constraints.mustPayToPubKey paramCompanyAddress prvCompanyFeeCredit                        
    --                 where
    --                 reportI = mkPayoutReportI companyFee adIsPayHighestLosing w adHighestLosingSubmit
    --                 reportV = toV adBidAssetClass reportI
    --                 PayoutReportV{..} = reportV
    --                 redeemer = mkRedeemerAuctionEndedWinnerYes reportI
    --                 winnerBidCredit = fromMaybe mempty prvWinnerBidCredit                      


    ledgerTx <- submitTxConstraintsWith lookups txC
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @String $ printf "closed auction %s for token %s"
        (show adAuction)
        (show cpAnchor)

 
mintAnchor :: Contract w AuctionSchema T.Text Anchor
mintAnchor = do
    pkh <- ownPubKeyHash

    sym <- fmap Currency.currencySymbol $ 
            mapError (T.pack . show @Currency.CurrencyError) $
                Currency.mintContract pkh [(anchorTokenName, 1)]

    pure $ Anchor sym


findViaAnchor :: Anchor -> Contract w s T.Text (Maybe (TxOutRef, ChainIndexTxOut, AuctionDatum))
findViaAnchor anchorSymbol = do
    utxos <- Map.filter f <$> utxosTxOutTxAt myAddress
    pure $ case Map.toList utxos of
        [(oref, (o, citx))] -> (oref, o,) <$> auctionDatum (toTxOut o) (\dh -> Map.lookup dh $ _citxData citx)
        _ -> Nothing
    where
        f :: (ChainIndexTxOut, Plutus.ChainIndex.Tx.ChainIndexTx) -> Bool
        f (o, _) = assetClassValueOf (txOutValue $ toTxOut o) (anchorAsset anchorSymbol) == 1                    
