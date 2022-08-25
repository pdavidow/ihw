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
    ( auctionAddress
    , auctionHash    
    , auctionValidator
    , typedAuctionValidator
    , typedValidator
    )    
    where

import           Data.Aeson.Types (Value(Bool))
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise       ( serialise )

import           Ledger
import           Ledger.Ada           as Ada

import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx.AssocMap as AssocMap
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import qualified PlutusTx
import           PlutusTx.Prelude 

import           Anchor 

import           Auction.BidderStatus
import           Auction.Share
import           Auction.Types


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


{-# INLINEABLE typedValidator #-}
typedValidator :: Scripts.TypedValidator Scripts.Any
typedValidator = Scripts.unsafeMkTypedValidator auctionValidator


{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator ad redeemer ctx = 
    traceIfFalse "wrong input value" correctInputValue &&

    case redeemer of
        Register pkh ->
            traceIfFalse "bidder is seller" (not $ isSeller pkh) &&
            traceIfFalse "bidder already registered" (not $ isBidderAtLeastRegistered pkh)

        Approve sellerPkh fitPkhs ->
            traceIfFalse "approver is not seller" (isSeller sellerPkh) &&
            traceIfFalse "empty list" (not $ null fitPkhs) &&
            traceIfFalse "not all are registered" (isAllRegisterd fitPkhs)

        MkBid b@Bid{..} ->
            traceIfFalse "bid too low"        (sufficientBid bBid)         &&
            traceIfFalse "wrong output datum" (correctBidOutputDatum b)    &&
            traceIfFalse "wrong output value" (correctBidOutputValue bBid) &&
            traceIfFalse "wrong refund"       correctBidRefund             &&
            traceIfFalse "too late"           correctBidSlotRange

        Close ->
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
    tokenValue = auctionedTokenValue auction

    correctInputValue :: Bool
    correctInputValue = inVal == 
        anchorValue (adAnchor ad) <> tokenValue <> 
            case adHighestBid ad of
                Nothing      -> Ada.lovelaceValueOf minLovelace
                Just Bid{..} -> Ada.lovelaceValueOf $ minLovelace + bBid

    isSeller :: PubKeyHash -> Bool
    isSeller pkh = aSeller auction == pkh      

    isBidderAtLeastRegistered :: PubKeyHash -> Bool      
    isBidderAtLeastRegistered pkh = AssocMap.member pkh $ aBidders auction

    isAllRegisterd :: [PubKeyHash] -> Bool 
    isAllRegisterd = all $ isBidderRegistered $ aBidders auction

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
        txOutValue ownOutput == anchorValue (adAnchor ad) <> tokenValue <> Ada.lovelaceValueOf (minLovelace + amount)

    correctBidRefund :: Bool
    correctBidRefund = case adHighestBid ad of
        Nothing      -> True
        Just Bid{..} ->
          let
            os = [ o
                 | o <- txInfoOutputs info
                 , txOutAddress o == pubKeyHashAddress bBidder 
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
        txOutAddress o == pubKeyHashAddress h 