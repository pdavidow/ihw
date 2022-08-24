{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Auction.PayoutReport
    ( PayoutReportI(..)
    , PayoutReportV(..)
    , mkPayoutReportI
    , mkPayoutReportV
    , toV
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Maybe (fromJust)
import           GHC.Generics (Generic)
import qualified Data.Bifunctor as BF
import qualified Prelude as H--askell      
import           Schema (ToSchema)
import           Ledger ( PubKeyHash, AssetClass, Value ) 
import           Ledger.Value ( assetClassValue )
import qualified PlutusTx
import           PlutusTx.Prelude                

import           Lib.NaturalNumber.NatGE0 (unNatGE0)
import           Lib.NaturalNumber.NatGE1 as N1 
import           Auction.Types
import           CompanyFee ( applyAsCeiling )  


data PayoutReportI = PayoutReportI 
    { priCompanyFeeCredit :: Integer 
    , priNetSellerCredit :: Integer
    , priWinnerBidCredit :: Maybe Integer    
    }
        deriving stock (H.Eq, H.Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)
        
PlutusTx.makeIsDataIndexed ''PayoutReportI [('PayoutReportI, 0)]
PlutusTx.makeLift ''PayoutReportI


data PayoutReportV = PayoutReportV 
    { prvCompanyFeeCredit :: Value 
    , prvNetSellerCredit :: Value
    , prvWinnerBidCredit :: Maybe Value    
    }
        deriving stock (H.Eq, H.Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''PayoutReportV [('PayoutReportV, 0)]
PlutusTx.makeLift ''PayoutReportV


mkPayoutReportI :: CompanyFee -> Bool -> Submit -> Maybe Submit -> PayoutReportI
mkPayoutReportI companyFee isPayHighestLosing (winnerBid, _) mbHighestLosing = 
    let      
        mbHighestLosingBid = fst <$> mbHighestLosing

        mbWinnerBidCredit = 
            if isPayHighestLosing then
                (winnerBid -) <$> mbHighestLosingBid
            else
                Nothing           

        grossSellerCredit =
            if isPayHighestLosing then 
                fromMaybe winnerBid mbHighestLosingBid
            else 
                winnerBid

        companyFeeCredit = applyAsCeiling (mkOk grossSellerCredit) companyFee   
        companyFeeCreditI = unNatGE0 companyFeeCredit 
        netSellerCreditI = grossSellerCredit - companyFeeCreditI  
    in
        PayoutReportI
            { priCompanyFeeCredit = companyFeeCreditI
            , priNetSellerCredit = netSellerCreditI
            , priWinnerBidCredit = mbWinnerBidCredit                  
            }   


mkPayoutReportV :: AssetClass -> CompanyFee -> Bool -> Submit -> Maybe Submit -> PayoutReportV 
mkPayoutReportV bidAssetClass a b c d = toV bidAssetClass $ mkPayoutReportI a b c d


toV :: AssetClass -> PayoutReportI -> PayoutReportV
toV c PayoutReportI{..} =
    PayoutReportV 
        { prvCompanyFeeCredit = assetClassValue c priCompanyFeeCredit
        , prvNetSellerCredit = assetClassValue c priNetSellerCredit
        , prvWinnerBidCredit = assetClassValue c <$> priWinnerBidCredit                 
        }                         