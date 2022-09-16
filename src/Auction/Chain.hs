{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Auction.Chain
    ( auctionAddress  
    , auctionClient
    , auctionValidator
    , typedAuctionValidator
    )    
    where


import           Ledger
import           Ledger.Ada as Ada ( lovelaceValueOf )
import           Ledger.Constraints as Constraints
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


{-# INLINABLE transition #-}
transition :: AuctionParams -> State AuctionDatum -> AuctionRedeemer -> Maybe (TxConstraints Void Void, State AuctionDatum)
transition AuctionParams{..} State{..} r = case (stateValue, stateData, r) of          

    (v, InProgress h bidders, Register pkh)  
        |  not (isSeller pkh apSeller) 
        -> case validateRegisteree bidders pkh of
            Nothing -> Nothing 
            Just reg -> 
                let 
                    bidders' = registerBidder bidders reg 
                    newState = State (InProgress h bidders') v   

                    constraints 
                        =  Constraints.mustBeSignedBy pkh     
                        <> Constraints.mustValidateIn (to apDeadline)  
                in
                    Just (constraints, newState)

    (v, InProgress h bidders, Approve approver approvees)  
        |  isSeller approver apSeller
        && notNull approvees
        && isAnyApprovals approvals
        -> Just (constraints, newState)
            where 
                (approvals, _, _) = validateApprovees bidders approvees
                bidders' = approveBidders bidders approvals
                newState = State (InProgress h bidders') v                  

                constraints 
                    =  Constraints.mustBeSignedBy approver  
                    <> Constraints.mustValidateIn (to apDeadline)   

    (_, InProgress h bidders, MkBid b@(Bid pkh n)) 
        |  (n >= maybe apMinBid (\x -> bBid x + 1) h) 
        && isBidderApproved bidders pkh
        -> Just (constraints, newState)
            where 
                h' = Just b 
                v' = auctionedTokenValue apAsset <> Ada.lovelaceValueOf (minLovelace + n)
                payBackPrev = \bid -> Constraints.mustPayToPubKey (bBidder bid) (Ada.lovelaceValueOf $ bBid bid)
                newState = State (InProgress h' bidders) v'   

                constraints 
                    =  Constraints.mustBeSignedBy pkh  
                    <> Constraints.mustValidateIn (to apDeadline)  
                    <> maybe mempty payBackPrev h

    (_, InProgress h _, Close) 
        -> Just (constraints, newState)
            where 
                seller = unSeller apSeller
                val = auctionedTokenValue apAsset <> Ada.lovelaceValueOf minLovelace
                newState = State Finished mempty  

                constraints 
                    =  Constraints.mustValidateIn (from apDeadline)  
                    <> ( case h of
                            Nothing 
                                -> Constraints.mustPayToPubKey seller val

                            Just x 
                                -> Constraints.mustPayToPubKey (bBidder x) val 
                                <> Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf $ bBid x) 
                       )

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