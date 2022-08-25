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

module Auction.FitForApprovals 
    ( FitForApprovals -- hide constructor
    , certifyApprovees
    , pkhsFor
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


newtype FitForApprovals = FitForApprovals [PubKeyHash] deriving P.Show


certifyApprovees :: Bidders -> [PubKeyHash] -> (FitForApprovals, NotRegistereds, AlreadyApproveds)
certifyApprovees m = foldr f (FitForApprovals [], NotRegistereds [], AlreadyApproveds [])
    where f = P.undefined


pkhsFor :: FitForApprovals -> [PubKeyHash]
pkhsFor (FitForApprovals xs) = xs