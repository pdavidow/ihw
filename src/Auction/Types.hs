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

module Auction.Types
    ( AlreadyApproveds(..)
    , ApproveParams(..)
    , Auction(..)
    , Auctioning
    , AuctionAction(..)
    , AuctionDatum(..)
    , Bid(..)
    , Bidders
    , FitForApprovals(..)
    , FitForRegistration(..)
    , NotRegistereds(..)
    , Status(..)
    , BidParams(..)
    , CloseParams(..)
    , RegisterParams(..)
    , StartParams(..)
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           Ledger 
import           Ledger.Value as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude 
import qualified Prelude as P   
import           Schema (ToSchema)

import           Anchor
     
newtype FitForRegistration = FitForRegistration PubKeyHash deriving P.Show
newtype FitForApprovals = FitForApprovals [PubKeyHash] deriving P.Show
newtype NotRegistereds = NotRegistereds [PubKeyHash] deriving P.Show
newtype AlreadyApproveds = AlreadyApproveds [PubKeyHash] deriving P.Show

data Auctioning


data Status = Registered | Approved
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq Status where
    {-# INLINABLE (==) #-}
    Registered == Registered = True
    Approved == Approved = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''Status [('Registered, 0), ('Approved, 1)]
PlutusTx.makeLift ''Status  


type Bidders = AssocMap.Map PubKeyHash Status


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


data AuctionAction 
    = Register FitForRegistration
    | Approve {aaSeller :: PubKeyHash, aaFits :: FitForApprovals}
    | MkBid Bid 
    | Close 
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

 
data RegisterParams = RegisterParams -- no newtype
    { rpAnchor :: !Anchor
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


data ApproveParams = ApproveParams
    { apApprovals :: ![PubKeyHash] -- todo: use Non Empty List
    , apAnchor :: !Anchor
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


data BidParams = BidParams
    { bpBid    :: !Integer
    , bpAnchor :: !Anchor
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


data CloseParams = CloseParams 
    { cpAnchorGraveyard :: !AnchorGraveyard
    , cpAnchor :: !Anchor
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


