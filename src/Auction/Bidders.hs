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
    , mkBidders   
    , isAnyApprovals
    , isBidderApproved
    , isAllRegisterd
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
import           GHC.Generics (Generic)
import           Ledger ( PubKeyHash ) 
import qualified PlutusTx
import           PlutusTx.Prelude
                    ( otherwise,
                    Bool(..),
                    Maybe(..),
                    Eq(..),
                    ($),
                    (.),
                    not,
                    all,
                    foldr,
                    null )
import qualified PlutusTx.AssocMap as AssocMap
import qualified Prelude as P   
import           Schema (ToSchema)


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


type BiddersMap = AssocMap.Map PubKeyHash Status


newtype Bidders = Bidders BiddersMap
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''Bidders


newtype Approvals = Approvals [PubKeyHash] 
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData)

PlutusTx.makeLift ''Approvals


newtype Registration = Registration PubKeyHash 
    deriving stock (P.Eq, P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData)

PlutusTx.makeLift ''Registration


newtype AlreadyApproveds = AlreadyApproveds [PubKeyHash] 
    deriving P.Show


newtype NotRegistereds = NotRegistereds [PubKeyHash] 
    deriving P.Show


{-# INLINABLE mkBidders #-}
mkBidders :: Bidders
mkBidders = Bidders AssocMap.empty


{-# INLINABLE mapFrom #-}
mapFrom :: Bidders -> BiddersMap
mapFrom (Bidders x) = x


{-# INLINABLE pkhForRegistration #-}
pkhForRegistration :: Registration -> PubKeyHash
pkhForRegistration (Registration x) = x


{-# INLINABLE pkhsForApprovals #-}
pkhsForApprovals :: Approvals -> [PubKeyHash]
pkhsForApprovals (Approvals xs) = xs


{-# INLINABLE isAnyApprovals #-}
isAnyApprovals :: Approvals -> Bool 
isAnyApprovals (Approvals xs) = not $ null xs


{-# INLINABLE isBidderRegistered #-}
isBidderRegistered :: Bidders -> PubKeyHash -> Bool 
isBidderRegistered b pkh = Just Registered == AssocMap.lookup pkh (mapFrom b)


{-# INLINABLE isBidderApproved #-}
isBidderApproved :: Bidders -> PubKeyHash -> Bool 
isBidderApproved b pkh = Just Approved == AssocMap.lookup pkh (mapFrom b)


{-# INLINABLE isAllRegisterd #-}
isAllRegisterd :: Bidders -> [PubKeyHash] -> Bool 
isAllRegisterd = all . isBidderRegistered 


{-# INLINABLE registerBidder #-}
registerBidder :: Bidders -> Registration -> Bidders
registerBidder b x = Bidders $ AssocMap.insert (pkhForRegistration x) Registered (mapFrom b)


{-# INLINABLE approveBidders #-}
approveBidders :: Bidders -> Approvals -> Bidders
approveBidders b x = Bidders $ foldr (`AssocMap.insert` Approved) (mapFrom b) $ pkhsForApprovals x


{-# INLINABLE validateRegisteree #-}
validateRegisteree :: Bidders -> PubKeyHash -> Maybe Registration
validateRegisteree b x
  | isBidderRegistered b x = Nothing 
  | isBidderApproved b x = Nothing 
  | otherwise = Just $ Registration x  


{-# INLINABLE validateApprovees #-}
validateApprovees :: Bidders -> [PubKeyHash] -> (Approvals, AlreadyApproveds, NotRegistereds)
validateApprovees b = foldr f (Approvals [], AlreadyApproveds [], NotRegistereds [])
    where f = \ x (Approvals as, AlreadyApproveds bs, NotRegistereds cs) ->
            if isBidderRegistered b x then    (Approvals $ x:as, AlreadyApproveds bs    , NotRegistereds cs    )
            else if isBidderApproved b x then (Approvals as    , AlreadyApproveds $ x:bs, NotRegistereds cs    )
            else                              (Approvals as    , AlreadyApproveds bs    , NotRegistereds $ x:cs)
