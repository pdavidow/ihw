{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Auction.Types
    ( ApproveParams(..)
    , Auction(..)
    , AuctionRedeemer(..)
    , AuctionDatum(..)
    , Bid(..)
    , BidParams(..)
    , Seller(..)
    , StartParams(..)
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import           Ledger ( PubKeyHash, POSIXTime ) 
import           Ledger.Value as Value ( TokenName, CurrencySymbol )
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude 
import qualified Prelude as P   
import           Schema (ToSchema)

import           Auction.Bidders ( Bidders, Approvals, Registration )


newtype Seller = Seller {unSeller :: PubKeyHash}
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''Seller

---------------------
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


-- data Auction = Auction
--     { aSeller   :: !Seller
--     , aBidders  :: !Bidders
--     , aDeadline :: !POSIXTime
--     , aMinBid   :: !Integer
--     , aCurrency :: !CurrencySymbol
--     , aToken    :: !TokenName
--     , aThreader :: !ThreadToken
--     } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

-- instance Eq Auction where
--     {-# INLINABLE (==) #-}
--     a == b = (aSeller   a == aSeller   b) &&
--              (aBidders  a == aBidders  b) &&    
--              (aDeadline a == aDeadline b) &&
--              (aMinBid   a == aMinBid   b) &&
--              (aCurrency a == aCurrency b) &&
--              (aToken    a == aToken    b)

-- PlutusTx.unstableMakeIsData ''Auction
-- PlutusTx.makeLift ''Auction

---------------------
data AuctionParams = AuctionParams
    { apSeller   :: !Seller
    , apDeadline :: !POSIXTime
    , apMinBid   :: !Integer
    , apCurrency :: !CurrencySymbol -- todo AssetClass
    , apToken    :: !TokenName      -- "      "
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq AuctionParams where
    {-# INLINABLE (==) #-}
    a == b = (apSeller   a == apSeller   b) &&
             (apDeadline a == apDeadline b) &&
             (apMinBid   a == apMinBid   b) &&
             (apCurrency a == apCurrency b) 

PlutusTx.unstableMakeIsData ''AuctionParams
PlutusTx.makeLift ''AuctionParams

---------------------
data AuctionDatum 
    = AuctionDatum
        { adHighestBid :: !(Maybe Bid)
        , adBidders  :: !Bidders
        , aThreader :: !ThreadToken
        } 
    | Finished
        deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum

instance Eq AuctionDatum where
    {-# INLINABLE (==) #-}
    AuctionDatum x y z == AuctionDatum x' y' z'= (x == x') && (y == y') && (z == z')
    Finished           == Finished             = True
    _                  == _                    = False

---------------------
data AuctionRedeemer 
    = Register !Registration
    | Approve PubKeyHash Approvals
    | MkBid !Bid 
    | Close 
    deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionRedeemer
PlutusTx.makeLift ''AuctionRedeemer

---------------------
data StartParams = StartParams
    { spDeadline :: !POSIXTime
    , spMinBid   :: !Integer
    , spCurrency :: !CurrencySymbol
    , spToken    :: !TokenName    
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
 
---------------------
data ApproveParams = ApproveParams
    { apApprovals :: ![PubKeyHash] -- todo: use Non Empty List
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

---------------------
data BidParams = BidParams
    { bpBid    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)