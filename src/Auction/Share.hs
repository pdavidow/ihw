{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
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

module Auction.Share
    ( Auction(..)
    , AuctionAction(..)
    , AuctionDatum(..)
    , Bid(..)
    , Bidders
    , BidderState(..)
    , BidParams(..)
    , CloseParams(..)
    , StartParams(..)
    , auctionDatum
    , auctionedTokenValue
    , bidderState
    , isBidderApproved
    , isBidderRegistered
    , minBid
    , minLovelace
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import           Ledger 
import           Ledger.Value as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude 
import qualified Prelude as P   
import           Schema (ToSchema)

import           Anchor
     

data BidderState = BidderRegistered | BidderApproved
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq BidderState where
    {-# INLINABLE (==) #-}
    BidderRegistered == BidderRegistered = True
    BidderApproved == BidderApproved = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''BidderState [('BidderRegistered, 0), ('BidderApproved, 1)]
PlutusTx.makeLift ''BidderState  


type Bidders = AssocMap.Map PubKeyHash BidderState


data Auction = Auction
    { aSeller   :: !PubKeyHash
    , aBidders  :: !Bidders
    , aDeadline :: !POSIXTime
    , aMinBid   :: !Integer
    , aCurrency :: !CurrencySymbol
    , aToken    :: !TokenName
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq Auction where
    {-# INLINABLE (==) #-}
    a == b = (aSeller   a == aSeller   b) &&
             (aDeadline a == aDeadline b) &&
             (aMinBid   a == aMinBid   b) &&
             (aCurrency a == aCurrency b) &&
             (aToken    a == aToken    b)

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction


data Bid = Bid
    { bBidder :: !PubKeyHash
    , bBid    :: !Integer
    } deriving P.Show

instance Eq Bid where
    {-# INLINABLE (==) #-}
    b == c = (bBidder b == bBidder c) &&
             (bBid    b == bBid    c)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid


data AuctionAction = MkBid Bid | Close
    deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionAction
PlutusTx.makeLift ''AuctionAction


data AuctionDatum = AuctionDatum
    { adAuction    :: !Auction
    , adHighestBid :: !(Maybe Bid)
    , adAnchor :: !Anchor
    } deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum


data StartParams = StartParams
    { spDeadline :: !POSIXTime
    , spMinBid   :: !Integer
    , spCurrency :: !CurrencySymbol
    , spToken    :: !TokenName    
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

 
data BidParams = BidParams
    { bpBid    :: !Integer
    , bpAnchor :: !Anchor
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


data CloseParams = CloseParams 
    { cpAnchorGraveyard :: !AnchorGraveyard
    , cpAnchor :: !Anchor
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


{-# INLINABLE minBid #-}
minBid :: AuctionDatum -> Integer
minBid AuctionDatum{..} = case adHighestBid of
    Nothing      -> aMinBid adAuction
    Just Bid{..} -> bBid + 1


{-# INLINABLE auctionDatum #-}
auctionDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AuctionDatum
auctionDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d


auctionedTokenValue :: Auction -> Value
auctionedTokenValue x = Value.singleton (aCurrency x) (aToken x) 1


minLovelace :: Integer
minLovelace = 2000000


bidderState :: PubKeyHash -> Bidders -> Maybe BidderState 
bidderState = AssocMap.lookup


isBidderRegistered :: PubKeyHash -> Bidders -> Bool 
isBidderRegistered pkh m = 
    case bidderState pkh m of
        Nothing -> False 
        Just x -> x == BidderRegistered


isBidderApproved :: PubKeyHash -> Bidders -> Bool 
isBidderApproved pkh m = 
    case bidderState pkh m of
        Nothing -> False 
        Just x -> x == BidderApproved

