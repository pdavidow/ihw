{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Auction.Offchain
    ( AuctionSchema
    , cancel
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

import           Anchor
                    ( Anchor(..),
                    anchorTokenName,
                    anchorAsset,
                    anchorValue ) 
import           Auction.Onchain
                   
import           Auction.Types
import           CompanyAddress (unCompanyAddress)
import           Auction.PayoutReport 
import           Auction.TypesAuctionRedeemer 
import           Auction.Utility ( bidErrorToText, bidVal, toHighestLosing ) 
import           Auction.ValidateBid (validateBid)
import           Duration
import           Lib.NaturalNumber.NatGE1 as N1 ( NatGE1(unNatGE1) ) 
import qualified Lib.NEPosValue as NEPV (toValue)


type AuctionSchema =
        Endpoint "start" (PubKeyHash, AuctionPrep)        
    .\/ Endpoint "bid" (PubKeyHash, Anchor, Bid)
    .\/ Endpoint "cancel" (PubKeyHash, Anchor, Duration, AnchorGraveyard)
    .\/ Endpoint "scheduleClose" (PubKeyHash, Anchor, CompanyFee, AnchorGraveyard)    


start :: (PubKeyHash, AuctionPrep) -> Contract (Last Anchor) AuctionSchema T.Text ()
start (paramCompanyAddress, AuctionPrep{..}) = do         
    logInfo @String ""
    logInfo @String "=============================================================================="
    logInfo @String "begin 'start'"
    self <- ownPubKeyHash

    let datum = AuctionDatum
            { adSeller = self
            , adAsset = NEPV.toValue apAsset
            , adDeadline = apDeadline
            , adIsPayHighestLosing = apPaymentStyle == HighestLosingBid
            , adIsCancelable = apIsCancelable
            , adReservePrice = unNatGE1 $ unReservePrice apReservePrice
            , adBidIncrement = unNatGE1 $ unBidIncrement apBidIncrement
            , adBidAssetClass = apBidAssetClass  
            , adHighestSubmit = Nothing
            , adHighestLosingSubmit = Nothing
            }             

    anchor <- mintAnchor
    let txC = Constraints.mustPayToTheScript (PlutusTx.toBuiltinData datum) (anchorValue anchor <> adAsset datum) 

    ledgerTx <- submitTxConstraints (typedValidator paramCompanyAddress) txC       
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    tell $ Last $ Just anchor -- broadcasted here, only after tx confirmed

    logInfo @String $ printf "end start %s" (show datum) 


scheduleClose :: (PubKeyHash, Anchor, CompanyFee, AnchorGraveyard)  -> Contract w AuctionSchema T.Text ()
scheduleClose (paramCompanyAddress, anchor, companyFee, graveyard) = do         
    logInfo @String ""
    logInfo @String "=============================================================================="
    logInfo @String "begin 'scheduleClose'"
    mbX <- findViaAnchor paramCompanyAddress anchor
    (_, _, oldDatum@AuctionDatum{..}) <- case mbX of
        Nothing -> throwError "anchor not found, possibly due to cancellation" -- todo ugly
        Just x -> pure x
    logInfo @String $ printf "found anchor with datum %s" $ show oldDatum        

    self <- ownPubKeyHash
    when (self == adSeller && adIsCancelable) $ throwError "Seller may not schedule close since this would block option to cancel"

    void $ awaitTime adDeadline 
    close (paramCompanyAddress, anchor, companyFee, graveyard)


bid :: (PubKeyHash, Anchor, Bid) -> Contract w AuctionSchema T.Text ()
bid (paramCompanyAddress, anchor, newBid) = do  
    logInfo @String ""
    logInfo @String "=============================================================================="
    logInfo @String "begin 'bid'"
    mbX <- findViaAnchor paramCompanyAddress anchor
    (oref, o, oldDatum@AuctionDatum{..}) <- case mbX of
        Nothing -> throwError "anchor not found, possibly due to cancellation" -- todo ugly
        Just x -> pure x
    logInfo @String $ printf "found anchor with datum %s" $ show oldDatum        

    self <- ownPubKeyHash

    let newBidderDebit = bidVal adBidAssetClass newBid      
            
    logInfo @String $ "# & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # newBid: " <> show newBid
    logInfo @String $ "# & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # & # newBidderDebit: " <> show newBidderDebit

    let newSubmit = (unNatGE1 $ unBid newBid, self)

    case validateBid oldDatum newSubmit of
        Just e -> logError $ bidErrorToText e
        Nothing -> do
            let oldBidderCreditConstraint = case adHighestSubmit of
                    Nothing -> mempty
                    Just (oldBid, oldBidder) -> Constraints.mustPayToPubKey oldBidder $ assetClassValue adBidAssetClass oldBid                        

            let redeemer = mkRedeemerSubmitBid anchor newSubmit

            let newDatum = oldDatum
                    { adHighestSubmit = Just newSubmit
                    , adHighestLosingSubmit = adHighestSubmit
                    }
                    
            let lookups = 
                    Constraints.typedValidatorLookups (typedValidator paramCompanyAddress) <>
                    Constraints.otherScript (mkMyScript paramCompanyAddress) <>
                    Constraints.unspentOutputs (Map.singleton oref o)

            let txC = 
                    Constraints.mustPayToTheScript (PlutusTx.toBuiltinData newDatum) (anchorValue anchor <> adAsset <> newBidderDebit) <>
                    Constraints.mustValidateIn (to adDeadline) <>                
                    Constraints.mustSpendScriptOutput oref redeemer <>                    
                    oldBidderCreditConstraint 

            ledgerTx <- submitTxConstraintsWith lookups txC
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ printf "Submitted bid %s" (show newBid) 
            logInfo @String "ended bid" 


cancel :: (PubKeyHash, Anchor, Duration, AnchorGraveyard) -> Contract w AuctionSchema T.Text ()
cancel (paramCompanyAddress, anchor, buffer, graveyard) = do   
    logInfo @String ""
    logInfo @String "=============================================================================="
    logInfo @String "begin 'cancel'"
    mbX <- findViaAnchor paramCompanyAddress anchor
    (oref, o, oldDatum@AuctionDatum{..}) <- case mbX of
        Nothing -> throwError "anchor not found"
        Just x -> pure x
    logInfo @String $ printf "found anchor with datum %s" $ show oldDatum        

    self <- ownPubKeyHash  

    when (self /= adSeller) $ throwError "only seller can cancel"
    unless adIsCancelable $ throwError "auction is not cancelable"

    let lookups = 
            Constraints.typedValidatorLookups (typedValidator paramCompanyAddress) <>
            Constraints.otherScript (mkMyScript paramCompanyAddress) <>            
            Constraints.unspentOutputs (Map.singleton oref o)

    let pBuffer = toPOSIX buffer
    let redeemer = mkRedeemerCancel pBuffer
    let contraints = 
                Constraints.mustValidateIn (to (adDeadline - pBuffer)) <>      
                Constraints.mustSpendScriptOutput oref redeemer <>
                Constraints.mustPayToPubKey adSeller adAsset   

    let txC = case adHighestSubmit of
            Nothing -> 
                contraints       

            Just (highestBid, highestBidder) ->
                contraints <>
                Constraints.mustPayToPubKey highestBidder refund   
                    where refund = assetClassValue adBidAssetClass highestBid                              

    ledgerTx <- submitTxConstraintsWith lookups txC
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    buryAnchor paramCompanyAddress anchor graveyard -- todo burnAnchor ?
    logInfo @String "end cancel" 


close :: (PubKeyHash, Anchor, CompanyFee, AnchorGraveyard) -> Contract w AuctionSchema T.Text ()
close (paramCompanyAddress, anchor, companyFee, graveyard) = do 
    logInfo @String ""
    logInfo @String "=============================================================================="
    logInfo @String "begin 'close'"
    mbX <- findViaAnchor paramCompanyAddress anchor
    (oref, o, datum@AuctionDatum{..}) <- case mbX of
        Nothing -> do
            let e = "anchor not found, possibly due to cancellation" -- todo ugly
            logError e
            throwError e
        Just x -> pure x
    logInfo @String $ printf "found anchor with datum %s" $ show datum                

    let lookups = 
            Constraints.typedValidatorLookups (typedValidator paramCompanyAddress) <>
            Constraints.otherScript (mkMyScript paramCompanyAddress) <>            
            Constraints.unspentOutputs (Map.singleton oref o)

    logInfo @String $ printf "adHighestSubmit %s" $ show adHighestSubmit
    let txC = case adHighestSubmit of
            Nothing ->           
                Constraints.mustValidateIn (from adDeadline) <>              
                Constraints.mustSpendScriptOutput oref redeemer <>
                Constraints.mustPayToPubKey adSeller adAsset       
                    where redeemer = Redeemer $ PlutusTx.toBuiltinData AuctionEndedWinnerNo         
                
            Just w@(_, winner) ->
                Constraints.mustValidateIn (from adDeadline) <>
                Constraints.mustSpendScriptOutput oref redeemer <>
                Constraints.mustPayToPubKey adSeller prvNetSellerCredit <>
                Constraints.mustPayToPubKey winner (adAsset <> winnerBidCredit) <>
                Constraints.mustPayToPubKey paramCompanyAddress prvCompanyFeeCredit                        
                    where
                    reportI = mkPayoutReportI companyFee adIsPayHighestLosing w adHighestLosingSubmit
                    reportV = toV adBidAssetClass reportI
                    PayoutReportV{..} = reportV
                    redeemer = mkRedeemerAuctionEndedWinnerYes reportI
                    winnerBidCredit = fromMaybe mempty prvWinnerBidCredit                      


    ledgerTx <- submitTxConstraintsWith lookups txC
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    buryAnchor paramCompanyAddress anchor graveyard -- todo burnAnchor ?
    logInfo @String "end close" 

 
mintAnchor :: Contract w AuctionSchema T.Text Anchor
mintAnchor = do
    logInfo @String ""
    logInfo @String "=============================================================================="
    logInfo @String "begin 'mintAnchor'"
    self <- ownPubKeyHash

    sym <- fmap Currency.currencySymbol $ 
            mapError (T.pack . show @Currency.CurrencyError) $
                Currency.mintContract self [(anchorTokenName, 1)]

    pure $ Anchor sym


findViaAnchor :: PubKeyHash  -> Anchor -> Contract w s T.Text (Maybe (TxOutRef, ChainIndexTxOut, AuctionDatum))
findViaAnchor paramCompanyAddress anchorSymbol = do
    utxos <- Map.filter f <$> utxosTxOutTxAt (myAddress paramCompanyAddress)
    pure $ case Map.toList utxos of
        [(oref, (o, citx))] -> (oref, o,) <$> auctionDatum (toTxOut o) (\dh -> Map.lookup dh $ _citxData citx)
        _ -> Nothing
    where
        f :: (ChainIndexTxOut, Plutus.ChainIndex.Tx.ChainIndexTx) -> Bool
        f (o, _) = assetClassValueOf (txOutValue $ toTxOut o) (anchorAsset anchorSymbol) == 1                    


buryAnchor :: PubKeyHash  -> Anchor -> AnchorGraveyard -> Contract w AuctionSchema T.Text ()
buryAnchor paramCompanyAddress anchor (AnchorGraveyard pkh) = do  
    logInfo @String ""
    logInfo @String "=============================================================================="
    logInfo @String "begin buryAnchor" 

    let lookups = 
            Constraints.typedValidatorLookups (typedValidator paramCompanyAddress) <>
            Constraints.otherScript (mkMyScript paramCompanyAddress)            

    let txC = Constraints.mustPayToPubKey pkh (anchorValue anchor)           
    
    ledgerTx <- submitTxConstraintsWith lookups txC
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "ended buryAnchor" 


endpoints :: Contract (Last Anchor) AuctionSchema T.Text ()
endpoints = 
    forever
    $ handleError logError
    $ awaitPromise
    $          endpoint @"start" start         
      `select` endpoint @"scheduleClose" scheduleClose 
      `select` endpoint @"bid" bid     
      `select` endpoint @"cancel" cancel 

