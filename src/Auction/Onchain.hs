{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Auction.Onchain
    ( auctionDatum
    , apiAuctionScript
    , auctionScriptAsShortBs
    , typedValidator
    , mkMyScript
    , myAddress
    ) 
    where

import           Data.Aeson.Types (Value(Bool))
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise       ( serialise )

import           Ledger

import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as LV
import qualified PlutusTx
import           PlutusTx.Prelude 

import           Anchor 
import           Auction.TypesAuctionRedeemer
import           Auction.Types
import           Auction.Utility ( info, isCorrectSlotRange, isValuePaidTo )



---------


{-# INLINABLE auctionDatum #-}
auctionDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AuctionDatum
auctionDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d


{-# INLINEABLE typedValidator #-}
typedValidator :: Scripts.TypedValidator Scripts.Any
typedValidator = Scripts.unsafeMkTypedValidator . mkMyScript


{-# INLINEABLE mkMyScript #-}
mkMyScript :: Validator
mkMyScript x = Ledger.mkValidatorScript $
    $$(PlutusTx.compile [|| validatorParam ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode x
    where validatorParam y = myWrapValidator (mkValidator y)


{-# INLINABLE myWrapValidator #-}
myWrapValidator
    :: forall d r p
    . (PlutusTx.UnsafeFromData d, PlutusTx.UnsafeFromData r, PlutusTx.UnsafeFromData p)
    => (d -> r -> p -> Bool)
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
myWrapValidator f d r p = check (f (PlutusTx.unsafeFromBuiltinData d) (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData p))


{-# INLINABLE myAddress #-}
myAddress :: Ledger.Address
myAddress = scriptAddress . mkMyScript


{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> Bool 
mkValidator rawDatum rawRedeemer rawContext = 
    let
        datum = PlutusTx.unsafeFromBuiltinData @AuctionDatum rawDatum
        redeemer = PlutusTx.unsafeFromBuiltinData @AuctionRedeemer rawRedeemer
        ctx = PlutusTx.unsafeFromBuiltinData @ScriptContext rawContext

        !txi = info ctx   
        isCreditSellerAsset = isValuePaidTo txi (adSeller datum) (adAsset datum)     
        isCorrectEndSlotRange = isCorrectSlotRange from (adDeadline datum) ctx      
    in
    case redeemer of
        SubmitBid anchorCS (newBid, newBidder) -> 
            isAnchored &&
            isSignedByNewBidder &&
            isCorrectBidSlotRange &&
            isBidMeetsReserve &&
            isBidMeetsIncrement && 
            isCreditOldBidderMaybe &&
            isLockedNewBid
            where      
                isAnchored = LV.assetClassValueOf (valueLockedBy txi $ ownHash ctx) (LV.AssetClass (anchorCS, anchorTokenName)) == 1
                isSignedByNewBidder = txSignedBy (info ctx) newBidder
                isCorrectBidSlotRange = isCorrectSlotRange to (adDeadline datum) ctx
                isBidMeetsReserve = newBid >= adReservePrice datum

                isBidMeetsIncrement = 
                    case adHighestSubmit datum of
                        Nothing -> True
                        Just (oldBid, _) -> newBid >= oldBid + adBidIncrement datum                 

                isCreditOldBidderMaybe = 
                    case adHighestSubmit datum of
                        Nothing -> True
                        Just (oldBid, oldBidder) -> isValuePaidTo txi oldBidder $ LV.assetClassValue (adBidAssetClass datum) oldBid

                isLockedNewBid = lockedAmt >= newBid
                    where 
                        val = valueLockedBy (info ctx) $ ownHash ctx
                        lockedAmt = LV.assetClassValueOf val (adBidAssetClass datum)


        Cancel t ->
            isCancelable &&
            isCorrectCancelSlotRange && 
            isCreditSellerAsset &&
            isCreditHighestBidderRefundMaybe          
            where 
                isCancelable = adIsCancelable datum
                isCorrectCancelSlotRange = isCorrectSlotRange to (adDeadline datum - t) ctx
                isCreditHighestBidderRefundMaybe = 
                    maybe True (\(highestBid, highestBidder) -> isValuePaidTo txi highestBidder (LV.assetClassValue (adBidAssetClass datum) highestBid)) $ adHighestSubmit datum


        AuctionEndedWinnerNo -> 
            isCorrectEndSlotRange && 
            isCreditSellerAsset            


        AuctionEndedWinnerYes companyFeeCredit netSellerCredit winnerBidCredit  -> 
            isCorrectEndSlotRange && 
            isCreditSeller &&
            isCreditWinnerAsset &&
            isCreditWinnerBidRefundMaybe &&    
            isCreditCompany   
            where     
                (_, winner) = fromMaybe neverDefault $ adHighestSubmit datum where neverDefault = (0, PubKeyHash "")      
                isCreditSeller = isValuePaidTo txi (adSeller datum) $ LV.assetClassValue (adBidAssetClass datum) netSellerCredit
                isCreditWinnerAsset = isValuePaidTo txi winner $ adAsset datum                              
                isCreditWinnerBidRefundMaybe = maybe True (isValuePaidTo txi winner . LV.assetClassValue (adBidAssetClass datum)) winnerBidCredit    
                isCreditCompany = isValuePaidTo txi paramCompanyAddress $ LV.assetClassValue (adBidAssetClass datum) companyFeeCredit                  


auctionScript :: Plutus.Script
auctionScript = Ledger.unValidatorScript . mkMyScript


auctionScriptAsShortBs :: SBS.ShortByteString
auctionScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . auctionScript


apiAuctionScript :: PlutusScript PlutusScriptV1
apiAuctionScript = PlutusScriptSerialised . auctionScriptAsShortBs                