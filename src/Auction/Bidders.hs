{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Auction.Bidders 
    ( Bidders -- hide constructor
    , approveBidders
    , isBidderApproved
    , isBidderRegistered
    , registerBidder
    , mapFrom
    )
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Ledger ( PubKeyHash ) 
import qualified PlutusTx
import           PlutusTx.Prelude ( Eq, ($), foldr  )
import qualified PlutusTx.AssocMap as AssocMap
import qualified Prelude as P   
import           Schema (ToSchema)
import           Auction.Status ( Status(..) )

import qualified Auction.CertApprovals as CA
import qualified Auction.CertRegistration as CR
import           Auction.Status ( Status(..) )


type BiddersMap = AssocMap.Map PubKeyHash Status

newtype Bidders = Bidders BiddersMap
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''Bidders


mapFrom :: Bidders -> BiddersMap
mapFrom (Bidders x) = x


{-# INLINABLE isBidderRegistered #-}
isBidderRegistered :: Bidders -> PubKeyHash -> Bool 
isBidderRegistered b pkh = Just Registered == AssocMap.lookup pkh (mapFrom b)


{-# INLINABLE isBidderApproved #-}
isBidderApproved :: Bidders -> PubKeyHash -> Bool 
isBidderApproved b pkh = Just Approved == AssocMap.lookup pkh (mapFrom b)


registerBidder :: Bidders -> CR.CertRegistration -> Bidders
registerBidder b x = Bidders $ AssocMap.insert (CR.pkhFor x) Registered (mapFrom b)


approveBidders :: Bidders -> CA.CertApprovals -> Bidders
approveBidders b x = Bidders $ foldr (`AssocMap.insert` Approved) (mapFrom b) $ CA.pkhsFor x