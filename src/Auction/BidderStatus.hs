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

module Auction.BidderStatus
    ( analyzeApprovees
    , approveBidders
    , isBidderApproved
    , isBidderRegistered
    , registerBidder
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

import           Auction.Share
import           Auction.Types


isBidderRegistered :: Bidders -> PubKeyHash -> Bool 
isBidderRegistered m pkh = maybe False (== Registered) $ AssocMap.lookup pkh m


isBidderApproved :: Bidders -> PubKeyHash -> Bool 
isBidderApproved m pkh = maybe False (== Approved) $ AssocMap.lookup pkh m


analyzeRegisteree :: PubKeyHash -> Either T.Text FitForRegistration
analyzeRegisteree pkh =


analyzeApprovees :: Bidders -> [PubKeyHash] -> (FitForApprovals, NotRegistereds, AlreadyApproveds)
analyzeApprovees m = foldr f (FitForApproval [], NotRegistered [], AlreadyApproved [])


-- registerBidder :: Bidders -> PubKeyHash -> Either T.Text Bidders
-- registerBidder m pkh =
--     if isBidderApproved m pkh then
--         Left "may not register already approved" 
--     else
--         Right $ AssocMap.insert pkh Registered m

registerBidder :: Bidders -> FitForRegistration -> Bidders
registerBidder m (FitForRegistration x) = AssocMap.insert x Registered m


approveBidders :: Bidders -> FitForApprovals -> Bidders
approveBidders m (FitForApprovals xs) = foldr (`AssocMap.insert` Approved) m xs


