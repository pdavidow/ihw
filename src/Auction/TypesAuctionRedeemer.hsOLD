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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Auction.TypesAuctionRedeemer
    ( AuctionRedeemer(..)
    , AuctionRedeemer'(..)
    , mkRedeemerAuctionEndedWinnerYes
    , mkRedeemerCancel
    , mkRedeemerSubmitBid
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import qualified Prelude as H--askell    

import           Ledger
import qualified PlutusTx
import           PlutusTx.Prelude
import           Anchor ( Anchor(..) )
import           Auction.PayoutReport 
import           Auction.Types
import           Lib.NaturalNumber.NatGE1 


data AuctionRedeemer'
    = SubmitBid' !Anchor !BidSubmit 
    | AuctionEndedWinnerNo' 
    | AuctionEndedWinnerYes' !PayoutReportV
        deriving stock (H.Eq, H.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''AuctionRedeemer' [('SubmitBid', 0), ('AuctionEndedWinnerNo', 1), ('AuctionEndedWinnerYes', 2) {-}, ('BurnAnchor', 3) -}]
PlutusTx.makeLift ''AuctionRedeemer'     


data AuctionRedeemer
    = SubmitBid 
        !CurrencySymbol -- AnchorCS
        !Submit -- BidSubmit

    | Cancel
        !POSIXTime -- buffer prior close when cancellation is no longer possible

    | AuctionEndedWinnerNo

    | AuctionEndedWinnerYes 
        !Integer -- priCompanyFeeCredit
        !Integer -- priNetSellerCredit
        !(Maybe Integer) -- priWinnerBidCredit

            deriving stock (H.Eq, H.Show, Generic)
            deriving anyclass (ToJSON, FromJSON)
         

PlutusTx.makeIsDataIndexed ''AuctionRedeemer [ ('SubmitBid, 0), ('Cancel, 1), ('AuctionEndedWinnerNo, 2), ('AuctionEndedWinnerYes, 3)]
PlutusTx.makeLift ''AuctionRedeemer 


mkRedeemerSubmitBid :: Anchor -> Submit -> Redeemer
mkRedeemerSubmitBid anchor x = Redeemer $ PlutusTx.toBuiltinData $ SubmitBid (unAnchor anchor) x


mkRedeemerCancel :: POSIXTime -> Redeemer
mkRedeemerCancel x = Redeemer $ PlutusTx.toBuiltinData $ Cancel x


mkRedeemerAuctionEndedWinnerYes :: PayoutReportI -> Redeemer
mkRedeemerAuctionEndedWinnerYes PayoutReportI{..} =
    Redeemer $ PlutusTx.toBuiltinData $ AuctionEndedWinnerYes
        priCompanyFeeCredit
        priNetSellerCredit
        priWinnerBidCredit  
