{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Auction.Onchain
    ( auctionAddress  
    , auctionClient
    , auctionValidator
    , typedAuctionValidator
    , Auction.Onchain.typedValidator
    )    
    where

import           GHC.Utils.Misc
import           Ledger
import           Ledger.Ada as Ada ( lovelaceValueOf )
import qualified Ledger.Typed.Scripts as Scripts  
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude

import           Auction.Bidders 
import           Auction.Share 
import           Auction.Types 


{-# INLINABLE isFinal #-}
isFinal :: AuctionDatum -> Bool
isFinal Finished = True
isFinal _        = False


{-# INLINABLE auctionStateMachine #-}
auctionStateMachine :: AuctionParams -> StateMachine AuctionDatum AuctionRedeemer
auctionStateMachine params = mkStateMachine (Just $ apAnchor params) (transition params) isFinal


{-# INLINABLE auctionDatum #-}
auctionDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AuctionDatum
auctionDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d


-- todo: throw errors? https://discord.com/channels/826816523368005654/826829805387120690/1018619653335023767
{-# INLINABLE transition #-}
transition :: AuctionParams -> State AuctionDatum -> AuctionRedeemer -> Maybe (TxConstraints Void Void, State AuctionDatum)
transition AuctionParams{..} State{..} r = case (stateValue, stateData, r) of 
    
    (v, InProgress h bidders, Register pkh)  
        |  not (isSeller pkh) 
        && eiReg isRight 
        -> Just (constraints, newState)
            where 
                eiReg = validateRegisteree bidders pkh
                bidders' = registerBidder bidders $ fromRight' eiReg -- partial is safe
                constraints 
                    =  Constraints.mustBeSignedBy pkh     
                    <> Constraints.mustValidateIn (to apDeadline)          
                newState = State (InProgress h bidders') v            

    (v, InProgress h bidders, Approve approver approvees)  
        |  isSeller approver
        && notNull approvees
        && notNull approvals
        -> Just (constraints, newState)
            where 
                (approvals, _, _) = validateApprovees bidders approvees
                bidders' = approveBidders bidders approvals
                constraints 
                    =  Constraints.mustBeSignedBy approver  
                    <> Constraints.mustValidateIn (to apDeadline)       
                newState = State (InProgress h bidders') v   

    (v, InProgress h bidders, MkBid (Bid pkh n)) 
        |  (n >= minBid stateData) 
        && isBidderApproved bidders pkh
        -> Just (constraints, newState)
            where 
                v' = auctionedTokenValue apAsset <> Ada.lovelaceValueOf (minLovelace + n)
                payBackPrev = \x -> Constraints.mustPayToPubKey (bBidder x) (Ada.lovelaceValueOf $ x bid)
                constraints 
                    =  Constraints.mustBeSignedBy pkh  
                    <> Constraints.mustValidateIn (to apDeadline)  
                    <> maybe mempty payBackPrev h
                newState = State (InProgress h bidders) v'   

    (v, InProgress h bidders, Close) 
        -> Just (constraints, newState)
            where 
                v' = auctionedTokenValue apAsset <> Ada.lovelaceValueOf (minLovelace + n)
                payBackPrev = \x -> Constraints.mustPayToPubKey (bBidder x) (Ada.lovelaceValueOf $ x bid)
                constraints 
                    =  Constraints.mustValidateIn (to apDeadline)  
                    <> maybe mempty payBackPrev h
                newState = State Finished v'  

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


auctionClient :: AuctionParams -> StateMachineClient AuctionDatum AuctionRedeemer
auctionClient params = mkStateMachineClient $ StateMachineInstance (auctionStateMachine params) (typedAuctionValidator params)


-- todo unused
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

