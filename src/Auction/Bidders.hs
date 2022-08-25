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

module Auction.Bidders 
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

import           Anchor
import           Auction.Share
import           Auction.Types


isBidderRegistered :: Bidders -> PubKeyHash -> Bool 
isBidderRegistered m pkh = maybe False (== Registered) $ AssocMap.lookup pkh m


isBidderApproved :: Bidders -> PubKeyHash -> Bool 
isBidderApproved m pkh = maybe False (== Approved) $ AssocMap.lookup pkh m


registerBidder :: Bidders -> PubKeyHash -> Either T.Text Bidders
registerBidder m pkh =
    if isBidderApproved m pkh then
        Left "may not register already approved" 
    else
        Right $ AssocMap.insert pkh Registered m


approveBidders :: Bidders -> [PubKeyHash] -> Bidders
approveBidders = foldr $ \ x acc -> 
    if isBidderRegistered acc x then 
        AssocMap.insert x Approved acc
    else
        acc

       
analyzeApprovees :: [PubKeyHash] -> Bidders -> ([PubKeyHash], [PubKeyHash], [PubKeyHash])
analyzeApprovees xs m = P.undefined