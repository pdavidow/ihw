{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Auction.Onchain
    ( auctionAddress
    , auctionHash    
    , auctionValidator
    , typedAuctionValidator
    , typedValidator
    )    
    where

import           Ledger
                    ( from,
                    to,
                    Datum(Datum),
                    TxOut(txOutDatumHash, txOutValue, txOutAddress),
                    Address,
                    Validator,
                    Value,
                    PubKeyHash (PubKeyHash),
                    pubKeyHashAddress,
                    scriptHashAddress,
                    findDatum,
                    getContinuingOutputs,
                    contains,
                    ScriptContext(scriptContextTxInfo),
                    TxInInfo(txInInfoResolved),
                    TxInfo(txInfoInputs, txInfoValidRange, txInfoOutputs),
                    ValidatorHash )
import           Ledger.Ada as Ada ( lovelaceValueOf )
import qualified PlutusTx.AssocMap as AssocMap
import qualified Ledger.Typed.Scripts as Scripts  
import qualified PlutusTx
import           PlutusTx.Prelude
                    ( Bool(..),
                    Integer,
                    Maybe(Just, Nothing),
                    AdditiveSemigroup((+)),
                    Eq((==)),
                    Ord((>=)),
                    Semigroup((<>)),
                    ($),
                    (.),
                    (&&),
                    maybe,
                    not,
                    all,
                    null,
                    traceError,
                    traceIfFalse ) 

import           Anchor ( anchorValue ) 
import           Auction.BidderStatusUtil ( isBidderRegistered, isBidderApproved )
import qualified Auction.CertApprovals as CA
import qualified Auction.CertRegistration as CR
import           Auction.Share ( minBid, minLovelace, auctionedTokenValue )
import           Auction.Types ( Auction(..), Bid(..), AuctionAction(..), AuctionDatum(..) )


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


{-# INLINEABLE typedValidator #-}
typedValidator :: Scripts.TypedValidator Scripts.Any
typedValidator = Scripts.unsafeMkTypedValidator auctionValidator


{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkAuctionValidator ad redeemer ctx = 
    traceIfFalse "wrong input value" correctInputValue &&

    case redeemer of
        Register cr ->
            traceIfFalse "bidder is seller" (not $ isSeller crPkh) &&
            traceIfFalse "bidder already registered or approved" (not $ isBidderAtLeastRegistered crPkh) &&
            traceIfFalse "wrong register output datum" (correctRegisterOutputDatum crPkh) &&
            traceIfFalse "wrong register output value" correctBidderStatusOutputValue        
            where crPkh = CR.pkhFor cr

        Approve sellerPkh ca ->
            traceIfFalse "approver is not seller" (isSeller sellerPkh) &&
            traceIfFalse "empty list" (not $ null caPkhs) &&
            traceIfFalse "not all are registered" (isAllRegisterd caPkhs) &&
            traceIfFalse "wrong approve output value" correctBidderStatusOutputValue              
            where caPkhs = CA.pkhsFor ca

        MkBid b@Bid{..} ->
            traceIfFalse "bidder not approved" (isBidderApproved (aBidders auction) bBidder) &&
            traceIfFalse "bid too low" (sufficientBid bBid) &&
            traceIfFalse "wrong bid output datum" (correctBidOutputDatum b) &&
            traceIfFalse "wrong bid output value" (correctBidOutputValue bBid) &&
            traceIfFalse "wrong refund" correctBidRefund &&
            traceIfFalse "too late" correctBidSlotRange

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


    correctBidderStatusOutputValue  :: Bool
    correctBidderStatusOutputValue =
        txOutValue ownOutput == anchorValue (adAnchor ad) <> tokenValue <> Ada.lovelaceValueOf (minLovelace + maybe 0 bBid (adHighestBid ad))


    correctRegisterOutputDatum :: PubKeyHash -> Bool
    correctRegisterOutputDatum pkh = 
        (adAuction outputDatum == auction {}) &&
        (adHighestBid outputDatum == adHighestBid ad) &&
        (adAnchor outputDatum == adAnchor ad)

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b = 
        (adAuction outputDatum == auction) &&
        (adHighestBid outputDatum == Just b) &&
        (adAnchor outputDatum == adAnchor ad)

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