{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Auction.Types
    ( ApproveParams(..)
    , Auction(..)
    , AuctionAction(..)
    , AuctionDatum(..)
    , Bid(..)
    , BidParams(..)
    , CloseParams(..)
    , RegisterParams(..)
    , StartParams(..)
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import           Ledger ( PubKeyHash, POSIXTime ) 
import           Ledger.Value as Value ( TokenName, CurrencySymbol )
import qualified PlutusTx
import           PlutusTx.Prelude ( Integer, Maybe, Eq(..), (&&) ) 
import qualified Prelude as P   
import           Schema (ToSchema)

import           Anchor ( AnchorGraveyard, Anchor )
import           Auction.Bidders ( Bidders )
import qualified Auction.CertApprovals as CA
import qualified Auction.CertRegistration as CR


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
             (aBidders  a == aBidders  b) &&    
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
    = Register !CR.CertRegistration
    | Approve {aaSeller :: !PubKeyHash, aaCerts :: !CA.CertApprovals}
    | MkBid !Bid 
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

 
newtype RegisterParams = RegisterParams 
    { rpAnchor :: Anchor
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
