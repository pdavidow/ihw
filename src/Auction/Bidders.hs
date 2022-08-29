{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Auction.Bidders 
    ( AlreadyApproveds(..)
    , Bidders -- hide constructor
    , Approvals -- hide constructor    
    , NotRegistereds(..)    
    , Registration -- hide constructor
    , approveBidders  
    , emptyBidders   
    , isBidderApproved
    , isAllRegisterd
    , isAtLeastRegistered
    , isBidderRegistered
    , registerBidder
    , mapFrom
    , pkhForRegistration
    , pkhsForApprovals    
    , validateApprovees
    , validateRegisteree       
    )
    where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Ledger ( PubKeyHash ) 
import qualified PlutusTx
import           PlutusTx.Prelude
                    ( otherwise,
                    Bool,
                    Maybe(Just),
                    Either(..),
                    ($),
                    (.),
                    all,
                    foldr,
                    Eq(..) ) 
import qualified PlutusTx.AssocMap as AssocMap
import qualified Prelude as P   
import           Schema (ToSchema)
import           Auction.Status ( Status(..) )


type BiddersMap = AssocMap.Map PubKeyHash Status

newtype Bidders = Bidders BiddersMap
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''Bidders


newtype Approvals = Approvals [PubKeyHash] 
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''Approvals


newtype Registration = Registration PubKeyHash 
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''Registration


newtype AlreadyApproveds = AlreadyApproveds [PubKeyHash] 
    deriving P.Show


newtype NotRegistereds = NotRegistereds [PubKeyHash] 
    deriving P.Show


mapFrom :: Bidders -> BiddersMap
mapFrom (Bidders x) = x


{-# INLINABLE pkhForRegistration #-}
pkhForRegistration :: Registration -> PubKeyHash
pkhForRegistration (Registration x) = x


{-# INLINABLE pkhsForApprovals #-}
pkhsForApprovals :: Approvals -> [PubKeyHash]
pkhsForApprovals (Approvals xs) = xs


emptyBidders :: Bidders
emptyBidders = Bidders AssocMap.empty


{-# INLINABLE isBidderRegistered #-}
isBidderRegistered :: Bidders -> PubKeyHash -> Bool 
isBidderRegistered b pkh = Just Registered == AssocMap.lookup pkh (mapFrom b)


{-# INLINABLE isBidderApproved #-}
isBidderApproved :: Bidders -> PubKeyHash -> Bool 
isBidderApproved b pkh = Just Approved == AssocMap.lookup pkh (mapFrom b)


isAllRegisterd :: Bidders -> [PubKeyHash] -> Bool 
isAllRegisterd = all . isBidderRegistered 


isAtLeastRegistered :: Bidders -> PubKeyHash -> Bool      
isAtLeastRegistered b pkh = AssocMap.member pkh (mapFrom b)


registerBidder :: Bidders -> Registration -> Bidders
registerBidder b x = Bidders $ AssocMap.insert (pkhForRegistration x) Registered (mapFrom b)


approveBidders :: Bidders -> Approvals -> Bidders
approveBidders b x = Bidders $ foldr (`AssocMap.insert` Approved) (mapFrom b) $ pkhsForApprovals x


validateRegisteree :: Bidders -> PubKeyHash -> Either T.Text Registration
validateRegisteree b x
  | isBidderRegistered b x = Left "already registered"
  | isBidderApproved b x = Left "already approved"
  | otherwise = Right $ Registration x


validateApprovees :: Bidders -> [PubKeyHash] -> (Approvals, AlreadyApproveds, NotRegistereds)
validateApprovees b = foldr f (Approvals [], AlreadyApproveds [], NotRegistereds [])
    where f = \ x (Approvals as, AlreadyApproveds bs, NotRegistereds cs) ->
            if isBidderRegistered b x then    (Approvals $ x:as, AlreadyApproveds bs    , NotRegistereds cs    )
            else if isBidderApproved b x then (Approvals as    , AlreadyApproveds $ x:bs, NotRegistereds cs    )
            else                              (Approvals as    , AlreadyApproveds bs    , NotRegistereds $ x:cs)
