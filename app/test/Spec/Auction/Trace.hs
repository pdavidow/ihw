{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Auction.Trace
    ( test1
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

import           Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Trace


walletSeller, walletBidderA, walletBidderB, walletBidderC, walletBidderD, walletBidderE, walletBidderF, walletGraveyard :: Wallet 
walletSeller    = w2 
walletBidderA   = w3 
walletBidderB   = w4 
walletBidderC   = w5 
walletBidderD   = w6 
walletBidderE   = w7 
walletBidderF   = w8 
walletGraveyard = w9 


slotCfg :: SlotConfig
slotCfg = def


emCfg :: Trace.EmulatorConfig
emCfg = Trace.EmulatorConfig (Left dist) def def
    where
        dist = Map.fromList 
            [ (walletSeller,  Ada.lovelaceValueOf 1_000_000_000)
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


getAnchor :: Trace.ContractHandle (Last Anchor) AuctionSchema T.Text -> Trace.EmulatorTrace Anchor
getAnchor h = do
    void $ Trace.waitNSlots 1
    l <- Trace.observableState h
    case l of
        Last Nothing -> Trace.waitNSlots 1 >> getAnchor h
        Last (Just x) -> Extras.logInfo (show x) >> return x


anchorGraveyard :: AnchorGraveyard
anchorGraveyard = AnchorGraveyard $ walletPubKeyHash walletGraveyard 


test1 :: IO ()
test1 = 
    Trace.runEmulatorTraceIO' def emCfg $ do
        hSeller <- Trace.activateContractWallet walletSeller endpoints          

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
        Trace.callEndpoint @"close" hSeller closeParams       

        void $ Trace.waitUntilTime $ spDeadline startParams    
        void $ Trace.waitNSlots 10    
