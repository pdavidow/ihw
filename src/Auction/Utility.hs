{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Auction.Utility
    ( bidderCreditVal
    , bidErrorToText
    , bidToI
    , bidVal
    , cancellationBuffer
    , info
    , isBiddingAlreadyStarted    
    , isCorrectSlotRange
    , isValuePaidTo
    , ownInput
    , ownOutput
    , toHighestLosing
    , auctionParams
    )
    where

import qualified Data.Text as T
import qualified Prelude as H--askell 

import           Ledger
                    ( findOwnInput,
                    getContinuingOutputs,
                    valuePaidTo,
                    contains,
                    ScriptContext(scriptContextTxInfo),
                    TxInInfo(txInInfoResolved),
                    TxInfo(txInfoValidRange),
                    PubKeyHash,
                    Interval,
                    POSIXTime,
                    TxOut,
                    AssetClass,
                    Value ) 
import           Ledger.Value ( assetClassValue, geq )
import           PlutusTx.Prelude
                    ( Bool,
                    Integer,
                    Maybe(..),
                    ($),
                    (.),
                    isJust,
                    traceError,
                    Monoid(mempty) )
import           Auction.Types
                    ( CompanyFee(CompanyFee),
                    Bid(unBid),
                    BidSubmit(..),
                    HighestSubmit(unHighestSubmit),
                    HighestLosingSubmit(HighestLosingSubmit),
                    ReservePrice(unReservePrice),
                    BidIncrement(unBidIncrement),
                    AuctionParams(..),
                    AuctionDatum(adHighestSubmit),
                    BidError(..) )
import           Lib.NaturalNumber.NatGE1 ( unNatGE1 ) 
import qualified PercentTimesTen as PctX10
import           CompanyAddress (testCompanyAddress)
import           Duration ( Duration(Hour) ) 
import           Utility (companyPkh)


auctionParams :: AuctionParams
auctionParams = AuctionParams
    { pCompanyAddress = companyPkh
    }


cancellationBuffer :: Duration
cancellationBuffer = Hour


{-# INLINABLE isBiddingAlreadyStarted #-}
isBiddingAlreadyStarted :: AuctionDatum -> Bool
isBiddingAlreadyStarted = isJust . adHighestSubmit


{-# INLINABLE bidErrorToText #-}
bidErrorToText :: BidError -> T.Text
bidErrorToText BelowReserve = T.pack "Below Reserve Price" 
bidErrorToText BelowIncrement = T.pack "Below Increment" 


{-# INLINABLE bidToI #-}
bidToI :: Bid -> Integer
bidToI = unNatGE1 . unBid


{-# INLINABLE bidVal #-}
bidVal :: AssetClass -> Bid -> Value
bidVal c = assetClassValue c . bidToI 


-- todo ugly
{-# INLINABLE bidderCreditVal #-}
bidderCreditVal :: AssetClass -> Maybe BidSubmit -> Value
bidderCreditVal c mbX = 
    case mbX of
        Nothing -> mempty 
        Just BidSubmit{..} -> bidVal c bsBid


{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput ctx = case findOwnInput ctx of
    Nothing -> traceError "input missing"
    Just i  -> txInInfoResolved i


{-# INLINABLE ownOutput #-}
ownOutput :: ScriptContext -> TxOut
ownOutput ctx = case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected exactly one output"


{-# INLINABLE info #-}
info :: ScriptContext -> TxInfo
info = scriptContextTxInfo


isValuePaidTo ::  TxInfo -> PubKeyHash -> Value -> Bool
isValuePaidTo x pkh val = valuePaidTo x pkh `geq` val


{-# INLINABLE isCorrectSlotRange #-}
isCorrectSlotRange :: (POSIXTime -> Interval POSIXTime) -> POSIXTime -> ScriptContext -> Bool
isCorrectSlotRange f deadline ctx = f deadline `contains` txInfoValidRange (info ctx)        
            

{-# INLINABLE toHighestLosing #-}
toHighestLosing ::  HighestSubmit ->  HighestLosingSubmit 
toHighestLosing = HighestLosingSubmit . unHighestSubmit