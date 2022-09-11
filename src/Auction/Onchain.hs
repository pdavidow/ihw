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
    , Auction.Onchain.typedValidator
    )    
    where

import           Ledger
                    ( pubKeyHashAddress,
                    scriptHashAddress,
                    findDatum,
                    getContinuingOutputs,
                    contains,
                    from,
                    to,
                    Address,
                    ScriptContext(scriptContextTxInfo),
                    TxInInfo(txInInfoResolved),
                    TxInfo(txInfoInputs, txInfoValidRange, txInfoOutputs),
                    PubKeyHash,
                    Datum(Datum),
                    Validator,
                    ValidatorHash,
                    TxOut(txOutDatumHash, txOutValue, txOutAddress),
                    Value )
import           Ledger.Ada as Ada ( lovelaceValueOf )
import qualified Ledger.Typed.Scripts as Scripts  
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude
                    ( Bool(..),
                    Integer,
                    Maybe(Just, Nothing),
                    ($),
                    (.),
                    (&&),
                    not,
                    null,
                    maybe,
                    traceError,
                    traceIfFalse,
                    Eq((==)),
                    AdditiveSemigroup((+)),
                    Ord((>=)),
                    Semigroup((<>)) )

import           Auction.Bidders ( Approvals, Registration, pkhForRegistration, pkhsForApprovals, isBidderApproved, isAllRegisterd, isAtLeastRegistered, registerBidder, approveBidders )
import           Auction.Share ( minBid, minLovelace, auctionedTokenValue )
import           Auction.Types ( Auction(..), Bid(..), AuctionRedeemer(..), AuctionDatum(..), Seller(..) )


{-# INLINABLE isFinal #-}
isFinal :: AuctionDatum -> Bool
isFinal Finished = True
isFinal _        = False


{-# INLINABLE auctionStateMachine #-}
auctionStateMachine :: AuctionParams -> StateMachine AuctionDatum AuctionRedeemer
auctionStateMachine params = mkStateMachine (Just $ apThreader params) (transition params) isFinal


{-# INLINABLE transition #-}
transition :: AuctionParams -> State AuctionDatum -> AuctionRedeemer -> Maybe (TxConstraints Void Void, State AuctionDatum)
transition params s r = case (stateValue s, stateData s, r) of 

-- traceIfFalse "wrong input value" correctInputValue &&
    -- case redeemer of
    --     Register reg ->
    --         traceIfFalse "registeree is seller" (not $ isSeller pkhR) &&
    --         traceIfFalse "registeree already registered or approved" (not $ isAtLeastRegistered (aBidders auction) pkhR) &&
    --         traceIfFalse "wrong register output datum" (correctRegisterOutputDatum reg) &&
    --         traceIfFalse "wrong register output value" correctBidderStatusOutputValue        
    --         where pkhR = pkhForRegistration reg

    (v, AuctionDatum auction _,            Register) ->


    (v, AuctionDatum auction _,            Approve approverPkh, approvals) ->
    (v, AuctionDatum auction mbHighestBid, MkBid bid) ->
    (v, AuctionDatum auction mbHighestBid, Close) ->
    _ -> Nothing

{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionParams -> AuctionDatum -> AuctionRedeemer -> ScriptContext -> Bool
mkAuctionValidator params = mkValidator $ auctionStateMachine params


type Auctioning = StateMachine AuctionDatum AuctionRedeemer


typedAuctionValidator :: AuctionParams -> Scripts.TypedValidator Auctioning
typedAuctionValidator params = Scripts.mkTypedValidator @Auctioning
    ($$(PlutusTx.compile [|| mkAuctionValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AuctionDatum @AuctionRedeemer


auctionValidator :: AuctionParams -> Validator
auctionValidator = Scripts.validatorScript . typedAuctionValidator


auctionAddress :: AuctionParams -> Ledger.Address
auctionAddress = scriptAddress . auctionValidator    


{-# INLINEABLE typedValidator #-}
typedValidator :: Scripts.TypedValidator Scripts.Any
typedValidator = Scripts.unsafeMkTypedValidator auctionValidator


{-# INLINABLE mkAuctionValidator' #-}
mkAuctionValidator' :: AuctionDatum -> AuctionRedeemer -> ScriptContext -> Bool
mkAuctionValidator' ad redeemer ctx = 
    traceIfFalse "wrong input value" correctInputValue &&

    case redeemer of
        Register reg ->
            traceIfFalse "registeree is seller" (not $ isSeller pkhR) &&
            traceIfFalse "registeree already registered or approved" (not $ isAtLeastRegistered (aBidders auction) pkhR) &&
            traceIfFalse "wrong register output datum" (correctRegisterOutputDatum reg) &&
            traceIfFalse "wrong register output value" correctBidderStatusOutputValue        
            where pkhR = pkhForRegistration reg

        Approve sellerPkh app ->
            traceIfFalse "approver is not seller" (isSeller sellerPkh) &&
            traceIfFalse "empty list" (not $ null pkhsA) &&
            traceIfFalse "not all are registered" (isAllRegisterd (aBidders auction) pkhsA) &&
            traceIfFalse "wrong approve output datum" (correctApproveOutputDatum app) &&            
            traceIfFalse "wrong approve output value" correctBidderStatusOutputValue              
            where pkhsA = pkhsForApprovals app

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
                    traceIfFalse "expected seller to get token" (getsValue (unSeller $ aSeller auction) $ tokenValue <> Ada.lovelaceValueOf minLovelace)
                Just Bid{..} ->
                    traceIfFalse "expected highest bidder to get token" (getsValue bBidder $ tokenValue <> Ada.lovelaceValueOf minLovelace) &&
                    traceIfFalse "expected seller to get highest bid" (getsValue (unSeller $ aSeller auction) $ Ada.lovelaceValueOf bBid)

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
       tokenValue <> 
            case adHighestBid ad of
                Nothing      -> Ada.lovelaceValueOf minLovelace
                Just Bid{..} -> Ada.lovelaceValueOf $ minLovelace + bBid

    isSeller :: PubKeyHash -> Bool
    isSeller pkh = unSeller (aSeller auction) == pkh      

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
        txOutValue ownOutput == tokenValue <> Ada.lovelaceValueOf (minLovelace + maybe 0 bBid (adHighestBid ad))

    correctRegisterOutputDatum :: Registration -> Bool
    correctRegisterOutputDatum x = 
        (adAuction outputDatum == auction {aBidders = aBidders'}) &&
        (adHighestBid outputDatum == adHighestBid ad) 
            where aBidders' = registerBidder (aBidders auction) x

    correctApproveOutputDatum :: Approvals -> Bool
    correctApproveOutputDatum x = 
        (adAuction outputDatum == auction {aBidders = aBidders'}) &&
        (adHighestBid outputDatum == adHighestBid ad) 
            where aBidders' = approveBidders (aBidders auction) x

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b = 
        (adAuction outputDatum == auction) &&
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


---------

