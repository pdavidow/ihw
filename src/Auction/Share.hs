{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Auction.Share
    ( Auction(..)
    , AuctionAction(..)
    , AuctionDatum(..)
    , Bid(..)
    , BidParams(..)
    , CloseParams(..)
    , StartParams(..)
    , auctionDatum
    , auctionedTokenValue
    , minBid
    , minLovelace
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import           Ledger
                    ( txOutDatum,
                    PubKeyHash,
                    Datum(Datum),
                    DatumHash,
                    POSIXTime,
                    TxOut,
                    CurrencySymbol,
                    TokenName,
                    Value ) 
import           Ledger.Value as Value ( singleton )
import qualified PlutusTx
import           PlutusTx.Prelude ( Integer, Maybe(..), AdditiveSemigroup((+)), Eq(..), (&&) ) 
import qualified Prelude as P   
import           Schema (ToSchema)

import           Anchor ( Anchor, AnchorGraveyard )
     
 
data Auction = Auction
    { aSeller   :: !PubKeyHash
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