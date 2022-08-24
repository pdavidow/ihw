{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
    ( AnchorGraveyard(..)
    , Auction(..)
    , AuctionDatum(..)
    , AuctionParams(..) 
    , AuctionPrep(..)
    , Bid(..)
    , Bidder(..)
    , BidError(..)
    , BidIncrement(..)
    , BidSubmit(..)
    , CompanyFee(..)
    , HighestSubmit(..)
    , HighestLosingSubmit(..)
    , ReservePrice(..)
    , PaymentStyle(..)
    , Submit
    , Seller(..)
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import           Ledger 
import qualified PlutusTx
import           PlutusTx.Prelude 
import qualified Prelude as H--askell      
import           Schema (ToSchema)

import           CompanyAddress (CompanyAddress)
import           CompanyFee ( CompanyFee(..) )
import           Lib.NaturalNumber.NatGE1 (NatGE1)  
import           Lib.NEPosValue (NEPosValue)


----------
newtype Bid = Bid {unBid :: NatGE1}
    deriving stock (H.Eq, H.Ord, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, Ord)

PlutusTx.makeIsDataIndexed ''Bid [('Bid, 0)]
PlutusTx.makeLift ''Bid   

----------
newtype Seller = Seller {unSeller :: PubKeyHash}
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)    
    deriving newtype (Eq)

PlutusTx.makeIsDataIndexed ''Seller [('Seller, 0)]
PlutusTx.makeLift ''Seller 

----------
newtype Bidder = Bidder {unBidder :: PubKeyHash}
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)    
    deriving newtype (Eq)

PlutusTx.makeIsDataIndexed ''Bidder [('Bidder, 0)]
PlutusTx.makeLift ''Bidder  

----------
data BidSubmit = BidSubmit
    { bsBid :: !Bid
    , bsBidder :: !Bidder
    }
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq BidSubmit where
    {-# INLINABLE (==) #-}
    x == y = (bsBid x == bsBid y) &&
             (bsBidder x == bsBidder y)

PlutusTx.makeIsDataIndexed ''BidSubmit [('BidSubmit, 0)]
PlutusTx.makeLift ''BidSubmit 

----------
newtype HighestSubmit = HighestSubmit {unHighestSubmit :: BidSubmit}
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)    
    deriving newtype (Eq)

PlutusTx.makeIsDataIndexed ''HighestSubmit [('HighestSubmit, 0)]
PlutusTx.makeLift ''HighestSubmit 

----------
newtype HighestLosingSubmit = HighestLosingSubmit {unHighestLosingSubmit :: BidSubmit}
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)    
    deriving newtype (Eq)

PlutusTx.makeIsDataIndexed ''HighestLosingSubmit [('HighestLosingSubmit, 0)]
PlutusTx.makeLift ''HighestLosingSubmit    

----------
newtype ReservePrice = ReservePrice {unReservePrice :: NatGE1}
    deriving stock (H.Eq, H.Ord, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, Ord)

PlutusTx.makeIsDataIndexed ''ReservePrice [('ReservePrice, 0)]
PlutusTx.makeLift ''ReservePrice  

----------
newtype BidIncrement = BidIncrement {unBidIncrement :: NatGE1}
    deriving stock (H.Eq, H.Ord, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, Ord)

PlutusTx.makeIsDataIndexed ''BidIncrement [('BidIncrement, 0)]
PlutusTx.makeLift ''BidIncrement       

----------
data PaymentStyle = HighestLosingBid | HighestWinningBid
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq PaymentStyle where
    {-# INLINABLE (==) #-}
    HighestLosingBid == HighestLosingBid = True
    HighestWinningBid == HighestWinningBid = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''PaymentStyle [('HighestLosingBid, 0), ('HighestWinningBid, 1)]
PlutusTx.makeLift ''PaymentStyle  

----------
newtype AuctionParams = AuctionParams
    { pCompanyAddress :: PubKeyHash
    }
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq AuctionParams where
    {-# INLINABLE (==) #-}
    x == y = pCompanyAddress x == pCompanyAddress y 

PlutusTx.makeIsDataIndexed ''AuctionParams [('AuctionParams, 0)]
PlutusTx.makeLift ''AuctionParams

----------
newtype AnchorGraveyard  = AnchorGraveyard {unAnchorGraveyard :: PubKeyHash}
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)    
    deriving newtype (Eq)

PlutusTx.makeIsDataIndexed ''AnchorGraveyard [('AnchorGraveyard, 0)]
PlutusTx.makeLift ''AnchorGraveyard 

----------
data AuctionPrep = AuctionPrep -- todo ? rename AuctionBasic
    { apAsset :: !NEPosValue -- allows for bundling
    , apDeadline :: !POSIXTime
    , apReservePrice :: !ReservePrice
    , apBidIncrement :: !BidIncrement 
    , apBidAssetClass :: !AssetClass
    , apPaymentStyle :: !PaymentStyle 
    , apIsCancelable :: !Bool    
    }
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq AuctionPrep where
    {-# INLINABLE (==) #-}
    x == y = (apAsset x == apAsset y) &&
             (apDeadline x == apDeadline y) &&
             (apReservePrice x == apReservePrice y) &&
             (apBidIncrement x == apBidIncrement y) &&
             (apBidAssetClass x == apBidAssetClass y) &&
             (apPaymentStyle x == apPaymentStyle y) &&
             (apIsCancelable x == apIsCancelable y)                                 

PlutusTx.makeIsDataIndexed ''AuctionPrep [('AuctionPrep, 0)]
PlutusTx.makeLift ''AuctionPrep

----------
data Auction = Auction
    { aSeller :: !Seller
    , aPrep :: !AuctionPrep
    }
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq Auction where
    {-# INLINABLE (==) #-}
    x == y = (aSeller x == aSeller y) &&             
             (aPrep x == aPrep y)

PlutusTx.makeIsDataIndexed ''Auction [('Auction, 0)]
PlutusTx.makeLift ''Auction

----------
data BidError = BelowReserve | BelowIncrement 

----------
type Submit = (Integer, PubKeyHash)

data AuctionDatum = AuctionDatum
    { adSeller :: !PubKeyHash
    , adAsset :: !Value
    , adDeadline :: !POSIXTime
    , adIsPayHighestLosing :: !Bool
    , adIsCancelable :: !Bool
    , adReservePrice :: !Integer
    , adBidIncrement :: !Integer
    , adBidAssetClass :: !AssetClass  
    , adHighestSubmit :: !(Maybe Submit)
    , adHighestLosingSubmit :: !(Maybe Submit)    
    } 
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Eq AuctionDatum where
    {-# INLINABLE (==) #-}
    x == y = (adSeller x == adSeller y) &&
             (adAsset x == adAsset y) &&
             (adDeadline x == adDeadline y) &&
             (adIsPayHighestLosing x == adIsPayHighestLosing y) &&
             (adIsCancelable x == adIsCancelable y) &&             
             (adReservePrice x == adReservePrice y) &&
             (adBidIncrement x == adBidIncrement y) &&
             (adBidAssetClass x == adBidAssetClass y) &&
             (adHighestSubmit x == adHighestSubmit y) &&
             (adHighestLosingSubmit x == adHighestLosingSubmit y) 

PlutusTx.makeIsDataIndexed ''AuctionDatum [('AuctionDatum, 0)]
PlutusTx.makeLift ''AuctionDatum