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
    deriving newtype (Eq)

PlutusTx.makeIsDataIndexed ''Seller [('Seller, 0)]
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

PlutusTx.makeIsDataIndexed ''Bid [('Bid, 0)]
PlutusTx.makeLift ''Bid

---------------------
data AuctionParams = AuctionParams
    { apSeller :: !Seller
    , apDeadline :: !POSIXTime
    , apMinBid :: !Integer 
    , apAsset :: !AssetClass
    , apAnchor :: !ThreadToken    
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''AuctionParams [('AuctionParams, 0)]
PlutusTx.makeLift ''AuctionParams

---------------------
data AuctionDatum 
    = InProgress
        { adHighestBid :: !(Maybe Bid)
        , adBidders :: !Bidders
        } 
    | Finished
        deriving (P.Show, P.Eq, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''AuctionDatum [('InProgress, 0), ('Finished, 1)]
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

PlutusTx.makeIsDataIndexed ''AuctionRedeemer [('Register, 0), ('Approve, 1), ('MkBid,  2), ('Close, 3)]
PlutusTx.makeLift ''AuctionRedeemer

---------------------
data StartParams = StartParams
    { spDeadline :: !POSIXTime
    , spMinBid :: !Integer
    , spAsset :: !AssetClass  
    } deriving (P.Show, P.Eq, Generic, ToJSON, FromJSON, ToSchema)
 