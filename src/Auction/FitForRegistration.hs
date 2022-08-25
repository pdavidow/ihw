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

module Auction.FitForRegistration
    ( FitForRegistration -- hide constructor
    , certifyRegisteree
    , pkhFor
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
import           Auction.Types


newtype FitForRegistration = FitForRegistration PubKeyHash deriving P.Show


certifyRegisteree :: Bidders -> PubKeyHash -> Either T.Text FitForRegistration
certifyRegisteree m x
  | isBidderRegistered m x = Left "already registered"
  | isBidderApproved m x = Left "already approved"
  | otherwise = Right $ FitForRegistration x


pkhFor :: FitForRegistration -> PubKeyHash
pkhFor (FitForRegistration x) = x