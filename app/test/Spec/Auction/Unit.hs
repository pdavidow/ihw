{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Auction.Unit
    ( tests
    ) where

import           Cardano.Crypto.Hash                as Crypto
import           Control.Lens                       hiding (elements)
import           Control.Monad                      (void, when)
import qualified Control.Monad.Freer                as Freer
import qualified Control.Monad.Freer.Error          as Freer
import           Control.Monad.Freer.Extras.Log     (LogLevel (..))
import           Control.Monad.Freer.Extras as Extras
import           Data.Default                       (Default (def))
import           Data.Either ( fromRight )
import qualified Data.Map as Map
import           Data.Monoid                        (Last (..))
import qualified Data.Text as T

import           Ledger                             (Ada, Slot (..), TokenName, Value)
import qualified Ledger.Ada                         as Ada
import           Plutus.Contract                    hiding (currentSlot)
import           Plutus.Contract.Test               hiding (not)
import qualified Streaming.Prelude                  as S
import qualified Wallet.Emulator.Folds              as Folds
import qualified Wallet.Emulator.Stream             as Stream
import           Wallet.Types

import           Ledger.Ada
import           Ledger.TimeSlot                    (SlotConfig)
import qualified Ledger.TimeSlot                    as TimeSlot
import qualified Ledger.Value                       as Value
import           Plutus.Contract.Test 
import           Plutus.Contract.Test.ContractModel
import qualified Plutus.Trace.Emulator          as Trace  
import           PlutusTx.Monoid                    (inv)
import qualified PlutusTx.Prelude                   as PlutusTx

import           Test.QuickCheck                    hiding ((.&&.))
import           Test.Tasty
import           Test.Tasty.QuickCheck              (testProperty)

import           Anchor
import           Auction.Offchain 
import           Auction.Share
import           Duration

import           Utility ( adaAssetClass, companyPkh )
import           Plutus.V1.Ledger.Ada ( adaSymbol )


walletSeller, walletBidderA, walletBidderB, walletBidderC, walletBidderD, walletBidderE, walletBidderF, walletGraveyard :: Wallet 
walletSeller    = w2 
walletBidderA   = w3 
walletBidderB   = w4 
walletBidderC   = w5 
walletBidderD   = w6 
walletBidderE   = w7 
walletBidderF   = w8 
walletCloser    = w9
walletGraveyard = w10


slotCfg :: SlotConfig
slotCfg = def


emCfg :: Trace.EmulatorConfig
emCfg = Trace.EmulatorConfig (Left dist) def def
    where
        dist = Map.fromList 
            [ (walletSeller,  Ada.lovelaceValueOf 1_000_000_000  <> theTokenVal)
            , (walletBidderA, Ada.lovelaceValueOf 1_000_000_000)
            , (walletBidderB, Ada.lovelaceValueOf 1_000_000_000)
            , (walletBidderC, Ada.lovelaceValueOf 1_000_000_000)    
            , (walletBidderD, Ada.lovelaceValueOf 1_000_000_000)       
            , (walletBidderE, Ada.lovelaceValueOf 1_000_000_000)     
            , (walletBidderF, Ada.lovelaceValueOf 1_000_000_000)      
            , (walletCloser,  Ada.lovelaceValueOf 1_000_000_000)                                                                                                          
            ]  


tokenCurrency :: Value.CurrencySymbol
tokenCurrency = Value.CurrencySymbol $ PlutusTx.toBuiltin $ Crypto.hashToBytes $ Crypto.hashWith @Crypto.Blake2b_256 id "ffff"


tokenName :: TokenName
tokenName = "token"


theTokenVal :: Value
theTokenVal = Value.singleton tokenCurrency tokenName 1


minAda :: Value
minAda = Ada.lovelaceValueOf minLovelace


getAnchor :: Trace.ContractHandle (Last Anchor) AuctionSchema T.Text -> Trace.EmulatorTrace Anchor
getAnchor h = do
    void $ Trace.waitNSlots 1
    l <- Trace.observableState h
    case l of
        Last Nothing -> Trace.waitNSlots 1 >> getAnchor h
        Last (Just x) -> Extras.logInfo (show x) >> return x


anchorGraveyard :: AnchorGraveyard
anchorGraveyard = AnchorGraveyard $ walletPubKeyHash walletGraveyard 


tests :: TestTree
tests = testGroup "Auction unit" 
    [ checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "No bid"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller (minAda <> theTokenVal)                     
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hCloser <- Trace.activateContractWallet walletCloser endpoints  

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = 100_000_000
                    , spCurrency = tokenCurrency
                    , spToken    = tokenName                   
                    }  

            Trace.callEndpoint @"start" hSeller startParams   
            anchor <- getAnchor hSeller 

            let closeParams = CloseParams 
                    { cpAnchorGraveyard = anchorGraveyard
                    , cpAnchor = anchor
                    }

            void $ Trace.waitNSlots 10             
            Trace.callEndpoint @"close" hCloser closeParams       

            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 10


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "1 good bid total"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 1)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 9 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf 10) <> theTokenVal)  
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints               
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice nat2
--                     , apBidIncrement = BidIncrement nat1
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestWinningBid 
--                     , apIsCancelable = False                                        
--                     }               
--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)  

--             void $ Trace.waitNSlots 50
--             let bid = Bid nat10
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 50


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "1 bad bid total: late"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany mempty
--         .&&. walletFundsChange walletSeller mempty 
--         .&&. walletFundsChange walletBidderA mempty         
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints             
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 10_000_000
--                     , apReservePrice = ReservePrice nat2
--                     , apBidIncrement = BidIncrement nat1
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestWinningBid 
--                     , apIsCancelable = False                                         
--                     }          

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)           

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep

--             void $ Trace.waitNSlots 1 -- need at least one slot to bury anchor, else it will remain in seller's wallet
--             let bid = Bid nat10
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid)      


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "1 bad bid total: below reserve"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany mempty
--         .&&. walletFundsChange walletSeller mempty 
--         .&&. walletFundsChange walletBidderA mempty         
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints              
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice nat10
--                     , apBidIncrement = BidIncrement nat1
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestWinningBid 
--                     , apIsCancelable = False                                         
--                     }   

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)           

--             void $ Trace.waitNSlots 1
--             let bid = Bid nat9
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 10            


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "1 good bid total: at reserve"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 1)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 9 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf 10) <> theTokenVal)      
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints             
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice nat10
--                     , apBidIncrement = BidIncrement nat1
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestWinningBid 
--                     , apIsCancelable = False                                         
--                     }           

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)           

--             void $ Trace.waitNSlots 1
--             let bid = Bid nat10
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 10              


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "1 good bid total: above reserve, below increment"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 2)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 18 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf 20) <> theTokenVal)      
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints             
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice nat10
--                     , apBidIncrement = BidIncrement nat15
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestWinningBid 
--                     , apIsCancelable = False                                        
--                     }      

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)           

--             void $ Trace.waitNSlots 1
--             let bid = Bid nat20
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 10      


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "2 bids total: 1st good; 2nd bad, below increment"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 1)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 9 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf 10) <> theTokenVal)      
--         .&&. walletFundsChange walletBidderB mempty
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints             
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints
--             hBidderB <- Trace.activateContractWallet walletBidderB endpoints            

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice nat10
--                     , apBidIncrement = BidIncrement nat3
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestWinningBid 
--                     , apIsCancelable = False                                      
--                     }      

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)            

--             void $ Trace.waitNSlots 5
--             let bid1 = Bid nat10
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid1)

--             void $ Trace.waitNSlots 5
--             let bid2 = Bid nat12
--             Trace.callEndpoint @"bid" hBidderB (companyPkh, anchor, bid2)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 10     


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "2 good bids total: 2nd above increment"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 2)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 18 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA mempty        
--         .&&. walletFundsChange walletBidderB (inv (Ada.lovelaceValueOf 20) <> theTokenVal)      
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints             
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints
--             hBidderB <- Trace.activateContractWallet walletBidderB endpoints            

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice nat4
--                     , apBidIncrement = BidIncrement nat6
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestWinningBid 
--                     , apIsCancelable = False                                      
--                     }               

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)             

--             void $ Trace.waitNSlots 50
--             let bid1 = Bid nat7 --nat4
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid1)

--             void $ Trace.waitNSlots 50
--             let bid2 = Bid nat20
--             Trace.callEndpoint @"bid" hBidderB (companyPkh, anchor, bid2)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 50    


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "2 good bids total: 2nd above increment -- millions of lovelace"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 2_000_000)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 18_000_000 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA mempty        
--         .&&. walletFundsChange walletBidderB (inv (Ada.lovelaceValueOf 20_000_000) <> theTokenVal)      
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints              
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints
--             hBidderB <- Trace.activateContractWallet walletBidderB endpoints            

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice $ N1.mkOk 4_000_000
--                     , apBidIncrement = BidIncrement $ N1.mkOk 6_000_000
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestWinningBid 
--                     , apIsCancelable = False                                        
--                     }               

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)              

--             void $ Trace.waitNSlots 50
--             let bid1 = Bid $ N1.mkOk 7_000_000
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid1)

--             void $ Trace.waitNSlots 50
--             let bid2 = Bid $ N1.mkOk 20_000_000
--             Trace.callEndpoint @"bid" hBidderB (companyPkh, anchor, bid2)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 50  


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "1 good bid: winner pays HighestLosingBid"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 1)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 9 <> inv theTokenVal)   
--         .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf 10) <> theTokenVal)      
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints               
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints       

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice nat4
--                     , apBidIncrement = BidIncrement nat6
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestLosingBid 
--                     , apIsCancelable = False                                       
--                     }  

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)              

--             void $ Trace.waitNSlots 50
--             let bid1 = Bid nat10
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid1)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 50      


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "2 good bids total: winner pays HighestLosingBid"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 1)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 9 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA mempty        
--         .&&. walletFundsChange walletBidderB (inv (Ada.lovelaceValueOf 10) <> theTokenVal)      
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints              
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints
--             hBidderB <- Trace.activateContractWallet walletBidderB endpoints            

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice nat4
--                     , apBidIncrement = BidIncrement nat6
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestLosingBid 
--                     , apIsCancelable = False                                        
--                     }     

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)              

--             void $ Trace.waitNSlots 50
--             let bid1 = Bid nat10
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid1)

--             void $ Trace.waitNSlots 50
--             let bid2 = Bid nat20
--             Trace.callEndpoint @"bid" hBidderB (companyPkh, anchor, bid2)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 50              


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "3 good bids total: winner pays HighestLosingBid"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 1)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 9 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA mempty      
--         .&&. walletFundsChange walletBidderB mempty            
--         .&&. walletFundsChange walletBidderC (inv (Ada.lovelaceValueOf 10) <> theTokenVal)      
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints              
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints
--             hBidderB <- Trace.activateContractWallet walletBidderB endpoints            
--             hBidderC <- Trace.activateContractWallet walletBidderC endpoints  

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice nat4
--                     , apBidIncrement = BidIncrement nat1
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestLosingBid 
--                     , apIsCancelable = False                                       
--                     }     

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)              
 
--             void $ Trace.waitNSlots 50
--             let bid1 = Bid nat5
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid1)

--             void $ Trace.waitNSlots 50
--             let bid2 = Bid nat10
--             Trace.callEndpoint @"bid" hBidderB (companyPkh, anchor, bid2)

--             void $ Trace.waitNSlots 50
--             let bid3 = Bid nat20
--             Trace.callEndpoint @"bid" hBidderC (companyPkh, anchor, bid3)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 50              


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "several bids, last is not highest"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 1_000)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 9_000 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA mempty      
--         .&&. walletFundsChange walletBidderB mempty   
--         .&&. walletFundsChange walletBidderC mempty          
--         .&&. walletFundsChange walletBidderD mempty 
--         .&&. walletFundsChange walletBidderE (inv (Ada.lovelaceValueOf 10_000) <> theTokenVal)                 
--         .&&. walletFundsChange walletBidderF mempty                 
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints                
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints
--             hBidderB <- Trace.activateContractWallet walletBidderB endpoints            
--             hBidderC <- Trace.activateContractWallet walletBidderC endpoints  
--             hBidderD <- Trace.activateContractWallet walletBidderD endpoints              
--             hBidderE <- Trace.activateContractWallet walletBidderE endpoints   
--             hBidderF <- Trace.activateContractWallet walletBidderF endpoints  

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice $ N1.mkOk 1_000 
--                     , apBidIncrement = BidIncrement $ N1.mkOk 100
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestWinningBid 
--                     , apIsCancelable = False                    
--                     }          

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)              

--             void $ Trace.waitNSlots 100
--             let bid1 = Bid $ N1.mkOk 1_003 
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid1)

--             void $ Trace.waitNSlots 100
--             let bid2 = Bid $ N1.mkOk 4_000
--             Trace.callEndpoint @"bid" hBidderB (companyPkh, anchor, bid2)

--             void $ Trace.waitNSlots 100
--             let bid3 = Bid $ N1.mkOk 9_000
--             Trace.callEndpoint @"bid" hBidderC (companyPkh, anchor, bid3)

--             void $ Trace.waitNSlots 100
--             let bid4 = Bid $ N1.mkOk 9_100
--             Trace.callEndpoint @"bid" hBidderD (companyPkh, anchor, bid4)

--             void $ Trace.waitNSlots 100
--             let bid5 = Bid $ N1.mkOk 10_000
--             Trace.callEndpoint @"bid" hBidderE (companyPkh, anchor, bid5)

--             void $ Trace.waitNSlots 100
--             let bid6 = Bid $ N1.mkOk 9_200
--             Trace.callEndpoint @"bid" hBidderF (companyPkh, anchor, bid6)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 50   


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "several bids, last is not highest, HighestLosingBid"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 910)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 8_190 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA mempty      
--         .&&. walletFundsChange walletBidderB mempty   
--         .&&. walletFundsChange walletBidderC mempty          
--         .&&. walletFundsChange walletBidderD mempty 
--         .&&. walletFundsChange walletBidderE (inv (Ada.lovelaceValueOf 9_100) <> theTokenVal)                 
--         .&&. walletFundsChange walletBidderF mempty                 
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints
--             hCompany <- Trace.activateContractWallet walletCompany endpoints              
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints
--             hBidderB <- Trace.activateContractWallet walletBidderB endpoints            
--             hBidderC <- Trace.activateContractWallet walletBidderC endpoints  
--             hBidderD <- Trace.activateContractWallet walletBidderD endpoints              
--             hBidderE <- Trace.activateContractWallet walletBidderE endpoints   
--             hBidderF <- Trace.activateContractWallet walletBidderF endpoints  

--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , apReservePrice = ReservePrice $ N1.mkOk 1_000 
--                     , apBidIncrement = BidIncrement $ N1.mkOk 100
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestLosingBid 
--                     , apIsCancelable = False                    
--                     }          

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hCompany (companyPkh, anchor, companyFee, anchorGraveyard)               

--             void $ Trace.waitNSlots 100
--             let bid1 = Bid $ N1.mkOk 1_003 
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid1)

--             void $ Trace.waitNSlots 100
--             let bid2 = Bid $ N1.mkOk 4_000
--             Trace.callEndpoint @"bid" hBidderB (companyPkh, anchor, bid2)

--             void $ Trace.waitNSlots 100
--             let bid3 = Bid $ N1.mkOk 9_000
--             Trace.callEndpoint @"bid" hBidderC (companyPkh, anchor, bid3)

--             void $ Trace.waitNSlots 100
--             let bid4 = Bid $ N1.mkOk 9_100
--             Trace.callEndpoint @"bid" hBidderD (companyPkh, anchor, bid4)

--             void $ Trace.waitNSlots 100
--             let bid5 = Bid $ N1.mkOk 10_000
--             Trace.callEndpoint @"bid" hBidderE (companyPkh, anchor, bid5)

--             void $ Trace.waitNSlots 100
--             let bid6 = Bid $ N1.mkOk 9_200
--             Trace.callEndpoint @"bid" hBidderF (companyPkh, anchor, bid6)

--             void $ Trace.waitUntilTime $ apDeadline auctionPrep
--             void $ Trace.waitNSlots 50              


--     , checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "Not cancelable, scheduleClose by seller, 1 good Bid, wait until deadline"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletCompany (Ada.lovelaceValueOf 1)
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf 9 <> inv theTokenVal)
--         .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf 10) <> theTokenVal)                   
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints          
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints
            
--             let auctionPrep = AuctionPrep 
--                     { apAsset = theToken
--                     , apDeadline = TimeSlot.scSlotZeroTime slotCfg + 10_000_000
--                     , apReservePrice = ReservePrice nat2
--                     , apBidIncrement = BidIncrement nat1
--                     , apBidAssetClass = adaAssetClass
--                     , apPaymentStyle = HighestWinningBid 
--                     , apIsCancelable = False                
--                     }    

--             Trace.callEndpoint @"start" hSeller (companyPkh, auctionPrep)     
--             anchor <- getAnchor hSeller 
--             Trace.callEndpoint @"scheduleClose" hSeller (companyPkh, anchor, companyFee, anchorGraveyard)           

--             void $ Trace.waitNSlots 10
--             let bid = Bid nat10
--             Trace.callEndpoint @"bid" hBidderA (companyPkh, anchor, bid)
      
--             void $ Trace.waitUntilTime $ apDeadline auctionPrep    
--             void $ Trace.waitNSlots 10               
    ]