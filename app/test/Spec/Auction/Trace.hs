{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Auction.Trace
    ( test1
    ) where

import           Cardano.Crypto.Hash as Crypto ( Blake2b_256, hashToBytes, hashWith )
import           Control.Monad                      (void)
import           Control.Monad.Freer.Extras as Extras ( logInfo )
import           Data.Default                       (Default (def))
import qualified Data.Map as Map
import           Data.Monoid                        (Last (..))
import qualified Data.Text as T

import           Ledger                             (TokenName, Value)
import qualified Ledger.Ada                         as Ada
import           Plutus.Contract.Test ( w10, w2, w3, w4, w5, w6, w7, w8, walletPubKeyHash, Wallet )
import           Ledger.TimeSlot                    (SlotConfig)
import qualified Ledger.TimeSlot                    as TimeSlot
import qualified Ledger.Value                       as Value
import qualified Plutus.Trace.Emulator              as Trace  
import qualified PlutusTx.Prelude                   as PlutusTx

import           Anchor ( AnchorGraveyard(..), Anchor )
import           Auction.Offchain ( endpoints, AuctionSchema ) 
import           Auction.Types ( CloseParams(..), BidParams(..), ApproveParams(..), StartParams(..) )


walletSeller, walletBidderA, walletBidderB, walletBidderC, walletBidderD, walletBidderE, walletBidderF, walletGraveyard :: Wallet 
walletSeller    = w2 -- W7ce812d
walletBidderA   = w3 -- Wc30efb7
walletBidderB   = w4 
walletBidderC   = w5 
walletBidderD   = w6 
walletBidderE   = w7 
walletBidderF   = w8
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
            ]  


tokenCurrency :: Value.CurrencySymbol
tokenCurrency = Value.CurrencySymbol $ PlutusTx.toBuiltin $ Crypto.hashToBytes $ Crypto.hashWith @Crypto.Blake2b_256 id "ffff"


tokenName :: TokenName
tokenName = "token"


theTokenVal :: Value
theTokenVal = Value.singleton tokenCurrency tokenName 1


lowestAcceptableBid :: Integer
lowestAcceptableBid = 100_000_000


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
test1 = Trace.runEmulatorTraceIO' def emCfg $ do
    hSeller <- Trace.activateContractWallet walletSeller endpoints          
    hBidderA <- Trace.activateContractWallet walletBidderA endpoints

    let startParams = StartParams 
            { spDeadline = TimeSlot.scSlotZeroTime slotCfg + 20_000
            , spMinBid   = lowestAcceptableBid
            , spCurrency = tokenCurrency
            , spToken    = tokenName                   
            }  
    Trace.callEndpoint @"start" hSeller startParams   
    anchor <- getAnchor hSeller 
    void $ Trace.waitNSlots 5    

    let approveParams = ApproveParams
            { apApprovals = [walletPubKeyHash walletBidderA]
            , apAnchor = anchor
            } 
--     Trace.callEndpoint @"approve" hSeller approveParams                     
    void $ Trace.waitNSlots 5 

    let bidParams = BidParams
            { bpBid    = lowestAcceptableBid
            , bpAnchor = anchor
            }
    Trace.callEndpoint @"bid" hBidderA bidParams 
    void $ Trace.waitNSlots 5       

    let closeParams = CloseParams 
            { cpAnchorGraveyard = anchorGraveyard
            , cpAnchor = anchor
            }                  
    Trace.callEndpoint @"close" hSeller closeParams       
    void $ Trace.waitUntilTime $ spDeadline startParams    
    void $ Trace.waitNSlots 5          