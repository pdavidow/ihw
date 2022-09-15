{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Auction.Unit
    ( tests
    ) where

import           Cardano.Crypto.Hash as Crypto ( Blake2b_256, hashToBytes, hashWith )
import           Control.Lens ( (&), (.~) )
import           Control.Monad                      (void)
import           Control.Monad.Freer.Extras as Extras ( logInfo )
import           Data.Default                       (Default (def))
import qualified Data.Map as Map
import           Data.Monoid                        (Last (..))
import qualified Data.Text as T

import           Ledger                            
import qualified Ledger.Ada                         as Ada
import           Plutus.Contract.Test
                        ( Wallet,
                        walletPubKeyHash,
                        (.&&.),
                        assertNoFailedTransactions,
                        checkPredicateOptions,
                        defaultCheckOptions,
                        emulatorConfig,
                        w10,
                        w2,
                        w3,
                        w4,
                        w5,
                        w6,
                        w7,
                        w8,
                        walletFundsChange )
import           Ledger.TimeSlot                    (SlotConfig)
import qualified Ledger.TimeSlot                    as TimeSlot
import qualified Ledger.Value                       as Value
import qualified Plutus.Trace.Emulator              as Trace  
import           PlutusTx.Monoid                    (inv)
import qualified PlutusTx.Prelude                   as PlutusTx

import           Test.Tasty ( TestTree, testGroup )

import           Auction.Offchain ( endpoints, AuctionSchema ) 
import           Auction.Share
import           Auction.Types 

walletSeller, walletBidderA, walletBidderB, walletBidderC, walletBidderD, walletBidderE, walletBidderF :: Wallet 
walletSeller    = w2 -- W7ce812d
walletBidderA   = w3 -- Wc30efb7
walletBidderB   = w4 
walletBidderC   = w5 
walletBidderD   = w6 
walletBidderE   = w7 
walletBidderF   = w8


pkhA, pkhB, pkhC, pkhD, pkhE, pkhF :: PubKeyHash
pkhA = walletPubKeyHash walletBidderA
pkhB = walletPubKeyHash walletBidderB
pkhC = walletPubKeyHash walletBidderC
pkhD = walletPubKeyHash walletBidderD
pkhE = walletPubKeyHash walletBidderE
pkhF = walletPubKeyHash walletBidderF


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
            ]  


tokenCurrency :: Value.CurrencySymbol
tokenCurrency = Value.CurrencySymbol $ PlutusTx.toBuiltin $ Crypto.hashToBytes $ Crypto.hashWith @Crypto.Blake2b_256 id "ffff"


tokenName :: TokenName
tokenName = "token"


theAssetClass :: AssetClass
theAssetClass = Value.assetClass tokenCurrency tokenName


theTokenVal :: Value
theTokenVal = Value.singleton tokenCurrency tokenName 1


lowestAcceptableBid :: Integer
lowestAcceptableBid = 100_000_000


getParams :: Trace.ContractHandle (Last AuctionParams) AuctionSchema T.Text -> Trace.EmulatorTrace AuctionParams
getParams h = do
    void $ Trace.waitNSlots 1
    l <- Trace.observableState h
    case l of
        Last Nothing -> Trace.waitNSlots 1 >> getParams h
        Last (Just x) -> Extras.logInfo (show x) >> return x


tests :: TestTree
tests = testGroup "Auction unit" 
    [ checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "No bid"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller mempty                   
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints           

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass              
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5     
       
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5    


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "1 bid just lower than min, registered & approved upfront"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller mempty  
        .&&. walletFundsChange walletBidderA mempty                           
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints

            let startParams = StartParams 
                        { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                        , spMinBid   = lowestAcceptableBid
                        , spAsset = theAssetClass                 
                        }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                    
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"approve" hSeller (params, [pkhA])                    
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, lowestAcceptableBid - 1)
            void $ Trace.waitNSlots 5         
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5   


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "1 bid at min, registered & approved upfront"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf (lowestAcceptableBid - minLovelace) <> inv theTokenVal)   
        .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf (lowestAcceptableBid - minLovelace)) <> theTokenVal)                            
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                 
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"approve" hSeller (params, [pkhA])                  
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, lowestAcceptableBid)
            void $ Trace.waitNSlots 5         
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5    


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "1 bid at min, neither registered nor approved"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller mempty
        .&&. walletFundsChange walletBidderA mempty                         
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                   
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    

            Trace.callEndpoint @"bid" hBidderA (params, lowestAcceptableBid) 
            void $ Trace.waitNSlots 5         
              
            Trace.callEndpoint @"close" hSeller params        
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5         


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "1 bid at min, yes registered but not approved"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller mempty
        .&&. walletFundsChange walletBidderA mempty                         
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                   
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
   
            Trace.callEndpoint @"register" hBidderA params                     
            void $ Trace.waitNSlots 5              

            Trace.callEndpoint @"bid" hBidderA (params, lowestAcceptableBid) 
            void $ Trace.waitNSlots 5         
               
            Trace.callEndpoint @"close" hSeller params        
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5     


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "1 bid at min, yes registered twice but not approved"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller mempty
        .&&. walletFundsChange walletBidderA mempty                         
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                   
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
   
            Trace.callEndpoint @"register" hBidderA params                     
            void $ Trace.waitNSlots 5     
            Trace.callEndpoint @"register" hBidderA params                     
            void $ Trace.waitNSlots 5                       

            Trace.callEndpoint @"bid" hBidderA (params, lowestAcceptableBid) 
            void $ Trace.waitNSlots 5         
               
            Trace.callEndpoint @"close" hSeller params        
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5   


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "1 bid at min, not registered but yes approved upfront"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller mempty
        .&&. walletFundsChange walletBidderA mempty         ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                 
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, lowestAcceptableBid)
            void $ Trace.waitNSlots 5         
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5    


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "1 bid at min, registered once, approved twice upfront"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf (lowestAcceptableBid - minLovelace) <> inv theTokenVal)   
        .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf (lowestAcceptableBid - minLovelace)) <> theTokenVal)                            
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                 
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"approve" hSeller (params, [pkhA])                  
            void $ Trace.waitNSlots 5  
            Trace.callEndpoint @"approve" hSeller (params, [pkhA])                  
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, lowestAcceptableBid)
            void $ Trace.waitNSlots 5         
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5     


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "1 bid at min, registered twice, approved twice upfront"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf (lowestAcceptableBid - minLovelace) <> inv theTokenVal)   
        .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf (lowestAcceptableBid - minLovelace)) <> theTokenVal)                            
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                 
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"approve" hSeller (params, [pkhA])                  
            void $ Trace.waitNSlots 5  
            Trace.callEndpoint @"approve" hSeller (params, [pkhA])                  
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, lowestAcceptableBid)
            void $ Trace.waitNSlots 5         
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5   


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "1 bid above min, bidder registered, bidder and another approved"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf (200_000_000 - minLovelace) <> inv theTokenVal)   
        .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf (200_000_000 - minLovelace)) <> theTokenVal)                            
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints           

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                 
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"approve" hSeller (params, [pkhA, pkhB])                               
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, 200_000_000)
            void $ Trace.waitNSlots 5         
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5   


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "2 bids higher than min, but second lower than first, yes approved upfront"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf (200_000_000 - minLovelace) <> inv theTokenVal)   
        .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf (200_000_000 - minLovelace)) <> theTokenVal)  
        .&&. walletFundsChange walletBidderB mempty             
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints           
            hBidderB <- Trace.activateContractWallet walletBidderB endpoints  

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                 
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"register" hBidderB params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"approve" hSeller (params, [pkhA, pkhB])                               
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, 200_000_000)
            void $ Trace.waitNSlots 5         

            Trace.callEndpoint @"bid" hBidderB (params, 200_000_000 - 1)
            void $ Trace.waitNSlots 5              
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5  


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "2 bids higher than min, second higher than first, yes approved upfront"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf (200_000_001 - minLovelace) <> inv theTokenVal)   
        .&&. walletFundsChange walletBidderA mempty         
        .&&. walletFundsChange walletBidderB (inv (Ada.lovelaceValueOf (200_000_001 - minLovelace)) <> theTokenVal)             
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints           
            hBidderB <- Trace.activateContractWallet walletBidderB endpoints  

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                 
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"register" hBidderB params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"approve" hSeller (params, [pkhA, pkhB])                               
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, 200_000_000)
            void $ Trace.waitNSlots 5         

            Trace.callEndpoint @"bid" hBidderB (params, 200_000_000 + 1)
            void $ Trace.waitNSlots 5              
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5  


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "2 bids: First lower than min, second at min, yes approved upfront"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf (lowestAcceptableBid - minLovelace) <> inv theTokenVal)   
        .&&. walletFundsChange walletBidderA mempty         
        .&&. walletFundsChange walletBidderB (inv (Ada.lovelaceValueOf (lowestAcceptableBid - minLovelace)) <> theTokenVal)             
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints           
            hBidderB <- Trace.activateContractWallet walletBidderB endpoints  

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                 
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"register" hBidderB params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"approve" hSeller (params, [pkhA, pkhB])                               
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, lowestAcceptableBid - 1)
            void $ Trace.waitNSlots 5         

            Trace.callEndpoint @"bid" hBidderB (params, lowestAcceptableBid)
            void $ Trace.waitNSlots 5              
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5   


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "5 bids higher than min, each higher than previous, yes approved upfront"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf (100_000_005 - minLovelace) <> inv theTokenVal)   
        .&&. walletFundsChange walletBidderA mempty         
        .&&. walletFundsChange walletBidderB mempty 
        .&&. walletFundsChange walletBidderC mempty 
        .&&. walletFundsChange walletBidderD mempty                         
        .&&. walletFundsChange walletBidderE (inv (Ada.lovelaceValueOf (100_000_005 - minLovelace)) <> theTokenVal)             
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints
            hBidderB <- Trace.activateContractWallet walletBidderB endpoints
            hBidderC <- Trace.activateContractWallet walletBidderC endpoints
            hBidderD <- Trace.activateContractWallet walletBidderD endpoints
            hBidderE <- Trace.activateContractWallet walletBidderE endpoints

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                 
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"register" hBidderB params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"register" hBidderC params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"register" hBidderD params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"register" hBidderE params                   
            void $ Trace.waitNSlots 5  
                                               
            Trace.callEndpoint @"approve" hSeller (params, [pkhA, pkhB, pkhC, pkhD, pkhE])                               
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, 100_000_000 + 1)
            void $ Trace.waitNSlots 5         

            Trace.callEndpoint @"bid" hBidderB (params, 100_000_000 + 2)
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderC (params, 100_000_000 + 3)
            void $ Trace.waitNSlots 5 

            Trace.callEndpoint @"bid" hBidderD (params, 100_000_000 + 4)
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderE (params, 100_000_000 + 5)
            void $ Trace.waitNSlots 5                                                               
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5                


    ,  checkPredicateOptions
        (defaultCheckOptions & (emulatorConfig .~ emCfg))
        "2 bids higher than min, second higher than first; yes approved first upfront, but approve second after first bid"
        ( assertNoFailedTransactions    
        .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf (200_000_001 - minLovelace) <> inv theTokenVal)   
        .&&. walletFundsChange walletBidderA mempty         
        .&&. walletFundsChange walletBidderB (inv (Ada.lovelaceValueOf (200_000_001 - minLovelace)) <> theTokenVal)              
        ) $ do
            hSeller <- Trace.activateContractWallet walletSeller endpoints          
            hBidderA <- Trace.activateContractWallet walletBidderA endpoints
            hBidderB <- Trace.activateContractWallet walletBidderB endpoints

            let startParams = StartParams 
                    { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
                    , spMinBid   = lowestAcceptableBid
                    , spAsset = theAssetClass                 
                    }  
            Trace.callEndpoint @"start" hSeller startParams   
            params <- getParams hSeller 
            void $ Trace.waitNSlots 5    
    
            Trace.callEndpoint @"register" hBidderA params                   
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"register" hBidderB params                   
            void $ Trace.waitNSlots 5  
                                               
            Trace.callEndpoint @"approve" hSeller (params, [pkhA])                               
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderA (params, 200_000_000)
            void $ Trace.waitNSlots 5         

            Trace.callEndpoint @"approve" hSeller (params, [pkhB])                               
            void $ Trace.waitNSlots 5  

            Trace.callEndpoint @"bid" hBidderB (params, 200_000_000 + 1)
            void $ Trace.waitNSlots 5                                                             
               
            Trace.callEndpoint @"close" hSeller params       
            void $ Trace.waitUntilTime $ spDeadline startParams    
            void $ Trace.waitNSlots 5   





--     ,  checkPredicateOptions
--         (defaultCheckOptions & (emulatorConfig .~ emCfg))
--         "2 bids higher than min, second higher than first, yes approved first only upfront, approve second after second bid"
--         ( assertNoFailedTransactions    
--         .&&. walletFundsChange walletSeller (Ada.lovelaceValueOf (200_000_000 - minLovelace) <> inv theTokenVal)          
--         .&&. walletFundsChange walletBidderA (inv (Ada.lovelaceValueOf (200_000_000 - minLovelace)) <> theTokenVal)     
--         .&&. walletFundsChange walletBidderB mempty                  
--         ) $ do
--             hSeller <- Trace.activateContractWallet walletSeller endpoints          
--             hBidderA <- Trace.activateContractWallet walletBidderA endpoints
--             hBidderB <- Trace.activateContractWallet walletBidderB endpoints

--             let startParams = StartParams 
--                     { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 1_000_000
--                     , spMinBid   = lowestAcceptableBid
--                     , spCurrency = tokenCurrency
--                     , spToken    = tokenName                   
--                     }  
--             Trace.callEndpoint @"start" hSeller startParams   
--             anchor <- getAnchor hSeller 
--             void $ Trace.waitNSlots 5    

--             let registerParams = RegisterParams 
--                     { rpAnchor = anchor
--                     }     
--             Trace.callEndpoint @"register" hBidderA registerParams                     
--             void $ Trace.waitNSlots 5  

--             Trace.callEndpoint @"register" hBidderB registerParams                     
--             void $ Trace.waitNSlots 5  

--             let approveParamsA = ApproveParams
--                     { apApprovals = [walletPubKeyHash walletBidderA]
--                     , apAnchor = anchor
--                     } 
--             Trace.callEndpoint @"approve" hSeller approveParamsA                
--             void $ Trace.waitNSlots 5  

--             let bidParamsA = BidParams
--                     { bpBid    = 200_000_000
--                     , bpAnchor = anchor
--                     }
--             Trace.callEndpoint @"bid" hBidderA bidParamsA  
--             void $ Trace.waitNSlots 5     

--             let bidParamsB = BidParams
--                     { bpBid    = 200_000_000 + 1
--                     , bpAnchor = anchor
--                     }
--             Trace.callEndpoint @"bid" hBidderB bidParamsB 
--             void $ Trace.waitNSlots 5    

--             let approveParamsB = ApproveParams
--                     { apApprovals = [walletPubKeyHash walletBidderB]
--                     , apAnchor = anchor
--                     } 
--             Trace.callEndpoint @"approve" hSeller approveParamsB                   
--             void $ Trace.waitNSlots 5  

--             let closeParams = CloseParams 
--                     { cpAnchorGraveyard = anchorGraveyard
--                     , cpAnchor = anchor
--                     }                  
--             Trace.callEndpoint @"close" hSeller closeParams       
--             void $ Trace.waitUntilTime $ spDeadline startParams    
--             void $ Trace.waitNSlots 5                 
    ] 