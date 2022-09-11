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

import           Ledger  
import           Ledger.Value as Value 
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


---------------------
data AuctionParams = AuctionParams
    { apSeller :: !Seller
    , apDeadline :: !POSIXTime
    , apMinBid :: !Integer
    , apAsset :: !AssetClass
    , apThreader :: !ThreadToken    
    } deriving (P.Show, P.Eq, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''AuctionParams

---------------------
data AuctionDatum 
    = InProgress
        { adHighestBid :: !(Maybe Bid)
        , adBidders :: !Bidders
        } 
    | Finished
        deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum

instance Eq AuctionDatum where
    {-# INLINABLE (==) #-}
    InProgress x y == InProgress x' y' = (x == x') && (y == y')
    Finished == Finished = True
    _ == _ = False

---------------------
data AuctionRedeemer 
    = Register !Registration
    | Approve PubKeyHash Approvals
    | MkBid !Bid 
    | Close 
    deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionRedeemer

---------------------
data StartParams = StartParams
    { spDeadline :: !POSIXTime
    , spMinBid   :: !Integer
    , spCurrency :: !CurrencySymbol
    , spToken    :: !TokenName    
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
 
---------------------
data ApproveParams = ApproveParams
    { apApprovals :: ![PubKeyHash] -- todo: use NonEmpty List
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

---------------------
data BidParams = BidParams
    { bpBid    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)