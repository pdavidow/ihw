{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Auction.Types
    ( AuctionDatum(..)
    , AuctionParams(..)
    , AuctionRedeemer(..)
    , Bid(..)
    , Seller(..)
    , StartParams(..)
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import           Ledger ( POSIXTime, PubKeyHash(PubKeyHash), AssetClass )  
import           Plutus.Contract.StateMachine ( ThreadToken )
import qualified PlutusTx
import           PlutusTx.Prelude ( Bool(..), Integer, Maybe, Eq(..), (&&) ) 
import qualified Prelude as P   
import           Schema (ToSchema)

import           Auction.Bidders ( Bidders )


newtype Seller = Seller {unSeller :: PubKeyHash}
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''Seller

---------------------
data Bid = Bid
    { bBidder :: !PubKeyHash
    , bBid    :: !Integer
    } deriving (P.Show, P.Eq, Generic, ToJSON, FromJSON, ToSchema)

instance Eq Bid where
    {-# INLINABLE (==) #-}
    x == y = (bBidder x == bBidder y) &&
             (bBid    x == bBid    y)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

---------------------
data AuctionParams = AuctionParams
    { apSeller :: !Seller
    , apDeadline :: !POSIXTime
    , apMinBid :: !Integer -- todo Nat-ish
    , apAsset :: !AssetClass
    , apAnchor :: !ThreadToken    
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''AuctionParams
PlutusTx.makeLift ''AuctionParams

---------------------
data AuctionDatum 
    = InProgress
        { adHighestBid :: !(Maybe Bid)
        , adBidders :: !Bidders
        } 
    | Finished
        deriving (P.Show, P.Eq, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum

instance Eq AuctionDatum where
    {-# INLINABLE (==) #-}
    InProgress x y == InProgress x' y' = (x == x') && (y == y')
    Finished == Finished = True
    _ == _ = False

---------------------
data AuctionRedeemer 
    = Register !PubKeyHash
    | Approve !PubKeyHash ![PubKeyHash]
    | MkBid !Bid 
    | Close 
    deriving (P.Show, P.Eq, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''AuctionRedeemer
PlutusTx.makeLift ''AuctionRedeemer

---------------------
data StartParams = StartParams
    { spDeadline :: !POSIXTime
    , spMinBid :: !Integer
    , spAsset :: !AssetClass  
    } deriving (P.Show, P.Eq, Generic, ToJSON, FromJSON, ToSchema)
 