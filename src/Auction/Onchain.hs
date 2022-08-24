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
    -- ( auctionDatum
    -- , apiAuctionScript
    -- , auctionScriptAsShortBs
    -- , typedValidator
    -- , mkMyScript
    -- , myAddress
    -- ) 
    ( auctionAddress
    , auctionHash    
    , auctionValidator
    , typedAuctionValidator
    )    
    where

import           Data.Aeson.Types (Value(Bool))
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise       ( serialise )

import           Ledger
import           Ledger.Ada           as Ada

import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified PlutusTx
import           PlutusTx.Prelude 

import           Anchor 
-- import           Auction.TypesAuctionRedeemer
import           Auction.Share
-- import           Auction.Utility ( info, isCorrectSlotRange, isValuePaidTo )


data Auctioning


instance Scripts.ValidatorTypes Auctioning where
    type instance RedeemerType Auctioning = AuctionAction
    type instance DatumType Auctioning = AuctionDatum


typedAuctionValidator :: Scripts.TypedValidator Auctioning
typedAuctionValidator = Scripts.mkTypedValidator @Auctioning
    $$(PlutusTx.compile [|| mkAuctionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AuctionDatum @AuctionAction


auctionValidator :: Validator
auctionValidator = Scripts.validatorScript typedAuctionValidator


auctionHash :: Ledger.ValidatorHash
auctionHash = Scripts.validatorHash typedAuctionValidator


auctionAddress :: Ledger.Address
auctionAddress = scriptHashAddress auctionHash    


{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator ad redeemer ctx =
    traceIfFalse "wrong input value" correctInputValue &&

    case redeemer of
        MkBid b@Bid{..} ->
            traceIfFalse "bid too low"        (sufficientBid bBid)         &&
            traceIfFalse "wrong output datum" (correctBidOutputDatum b)    &&
            traceIfFalse "wrong output value" (correctBidOutputValue bBid) &&
            traceIfFalse "wrong refund"       correctBidRefund             &&
            traceIfFalse "too late"           correctBidSlotRange

        Close           ->
            traceIfFalse "too early" correctCloseSlotRange &&
            case adHighestBid ad of
                Nothing      ->
                    traceIfFalse "expected seller to get token" (getsValue (aSeller auction) $ tokenValue <> Ada.lovelaceValueOf minLovelace)
                Just Bid{..} ->
                    traceIfFalse "expected highest bidder to get token" (getsValue bBidder $ tokenValue <> Ada.lovelaceValueOf minLovelace) &&
                    traceIfFalse "expected seller to get highest bid" (getsValue (aSeller auction) $ Ada.lovelaceValueOf bBid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxInInfo
    input =
      let
        isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _  -> True
        xs = [i | i <- txInfoInputs info, isScriptInput i]
      in
        case xs of
            [i] -> i
            _   -> traceError "expected exactly one script input"

    inVal :: Ledger.Value
    inVal = txOutValue . txInInfoResolved $ input

    auction :: Auction
    auction = adAuction ad

    tokenValue :: Ledger.Value
    tokenValue = anchorValue $ aAnchor auction -- Value.singleton (aCurrency auction) (aToken auction) 1

    correctInputValue :: Bool
    correctInputValue = inVal == case adHighestBid ad of
        Nothing      -> tokenValue <> Ada.lovelaceValueOf minLovelace
        Just Bid{..} -> tokenValue <> Ada.lovelaceValueOf (minLovelace + bBid)

    sufficientBid :: Integer -> Bool
    sufficientBid amount = amount >= minBid ad

    ownOutput   :: TxOut
    outputDatum :: AuctionDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b = (adAuction outputDatum == auction)   &&
                              (adHighestBid outputDatum == Just b)

    correctBidOutputValue :: Integer -> Bool
    correctBidOutputValue amount =
        txOutValue ownOutput == tokenValue <> Ada.lovelaceValueOf (minLovelace + amount)

    correctBidRefund :: Bool
    correctBidRefund = case adHighestBid ad of
        Nothing      -> True
        Just Bid{..} ->
          let
            os = [ o
                 | o <- txInfoOutputs info
                 , txOutAddress o == pubKeyHashAddress bBidder Nothing
                 ]
          in
            case os of
                [o] -> txOutValue o == Ada.lovelaceValueOf bBid
                _   -> traceError "expected exactly one refund output"

    correctBidSlotRange :: Bool
    correctBidSlotRange = to (aDeadline auction) `contains` txInfoValidRange info

    correctCloseSlotRange :: Bool
    correctCloseSlotRange = from (aDeadline auction) `contains` txInfoValidRange info

    getsValue :: PubKeyHash -> Ledger.Value -> Bool
    getsValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h Nothing

---------


-- {-# INLINABLE auctionDatum #-}
-- auctionDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AuctionDatum
-- auctionDatum o f = do
--     dh <- txOutDatum o
--     Datum d <- f dh
--     PlutusTx.fromBuiltinData d


-- {-# INLINEABLE typedValidator #-}
-- typedValidator :: Scripts.TypedValidator Scripts.Any
-- typedValidator = Scripts.unsafeMkTypedValidator . mkMyScript


-- {-# INLINEABLE mkMyScript #-}
-- mkMyScript :: Validator
-- mkMyScript x = Ledger.mkValidatorScript $
--     $$(PlutusTx.compile [|| validatorParam ||])
--         `PlutusTx.applyCode`
--             PlutusTx.liftCode x
--     where validatorParam y = myWrapValidator (mkValidator y)


-- {-# INLINABLE myWrapValidator #-}
-- myWrapValidator
--     :: forall d r p
--     . (PlutusTx.UnsafeFromData d, PlutusTx.UnsafeFromData r, PlutusTx.UnsafeFromData p)
--     => (d -> r -> p -> Bool)
--     -> BuiltinData
--     -> BuiltinData
--     -> BuiltinData
--     -> ()
-- myWrapValidator f d r p = check (f (PlutusTx.unsafeFromBuiltinData d) (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData p))


-- {-# INLINABLE myAddress #-}
-- myAddress :: Ledger.Address
-- myAddress = scriptAddress . mkMyScript


-- {-# INLINEABLE mkValidator #-}
-- mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> Bool 
-- mkValidator rawDatum rawRedeemer rawContext = 
--     let
--         datum = PlutusTx.unsafeFromBuiltinData @AuctionDatum rawDatum
--         redeemer = PlutusTx.unsafeFromBuiltinData @AuctionRedeemer rawRedeemer
--         ctx = PlutusTx.unsafeFromBuiltinData @ScriptContext rawContext

--         !txi = info ctx   
--         isCreditSellerAsset = isValuePaidTo txi (adSeller datum) (adAsset datum)     
--         isCorrectEndSlotRange = isCorrectSlotRange from (adDeadline datum) ctx      
--     in
--     case redeemer of
--         SubmitBid anchorCS (newBid, newBidder) -> 
--             isAnchored &&
--             isSignedByNewBidder &&
--             isCorrectBidSlotRange &&
--             isBidMeetsReserve &&
--             isBidMeetsIncrement && 
--             isCreditOldBidderMaybe &&
--             isLockedNewBid
--             where      
--                 isAnchored = LV.assetClassValueOf (valueLockedBy txi $ ownHash ctx) (LV.AssetClass (anchorCS, anchorTokenName)) == 1
--                 isSignedByNewBidder = txSignedBy (info ctx) newBidder
--                 isCorrectBidSlotRange = isCorrectSlotRange to (adDeadline datum) ctx
--                 isBidMeetsReserve = newBid >= adReservePrice datum

--                 isBidMeetsIncrement = 
--                     case adHighestSubmit datum of
--                         Nothing -> True
--                         Just (oldBid, _) -> newBid >= oldBid + adBidIncrement datum                 

--                 isCreditOldBidderMaybe = 
--                     case adHighestSubmit datum of
--                         Nothing -> True
--                         Just (oldBid, oldBidder) -> isValuePaidTo txi oldBidder $ LV.assetClassValue (adBidAssetClass datum) oldBid

--                 isLockedNewBid = lockedAmt >= newBid
--                     where 
--                         val = valueLockedBy (info ctx) $ ownHash ctx
--                         lockedAmt = LV.assetClassValueOf val (adBidAssetClass datum)


--         Cancel t ->
--             isCancelable &&
--             isCorrectCancelSlotRange && 
--             isCreditSellerAsset &&
--             isCreditHighestBidderRefundMaybe          
--             where 
--                 isCancelable = adIsCancelable datum
--                 isCorrectCancelSlotRange = isCorrectSlotRange to (adDeadline datum - t) ctx
--                 isCreditHighestBidderRefundMaybe = 
--                     maybe True (\(highestBid, highestBidder) -> isValuePaidTo txi highestBidder (LV.assetClassValue (adBidAssetClass datum) highestBid)) $ adHighestSubmit datum


--         AuctionEndedWinnerNo -> 
--             isCorrectEndSlotRange && 
--             isCreditSellerAsset            


--         AuctionEndedWinnerYes companyFeeCredit netSellerCredit winnerBidCredit  -> 
--             isCorrectEndSlotRange && 
--             isCreditSeller &&
--             isCreditWinnerAsset &&
--             isCreditWinnerBidRefundMaybe &&    
--             isCreditCompany   
--             where     
--                 (_, winner) = fromMaybe neverDefault $ adHighestSubmit datum where neverDefault = (0, PubKeyHash "")      
--                 isCreditSeller = isValuePaidTo txi (adSeller datum) $ LV.assetClassValue (adBidAssetClass datum) netSellerCredit
--                 isCreditWinnerAsset = isValuePaidTo txi winner $ adAsset datum                              
--                 isCreditWinnerBidRefundMaybe = maybe True (isValuePaidTo txi winner . LV.assetClassValue (adBidAssetClass datum)) winnerBidCredit    
--                 isCreditCompany = isValuePaidTo txi paramCompanyAddress $ LV.assetClassValue (adBidAssetClass datum) companyFeeCredit                  


-- auctionScript :: Plutus.Script
-- auctionScript = Ledger.unValidatorScript . mkMyScript


-- auctionScriptAsShortBs :: SBS.ShortByteString
-- auctionScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . auctionScript


-- apiAuctionScript :: PlutusScript PlutusScriptV1
-- apiAuctionScript = PlutusScriptSerialised . auctionScriptAsShortBs                