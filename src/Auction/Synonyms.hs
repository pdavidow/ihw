module Auction.Synonyms
    ( BiddersMap
    ) 
    where

import           Ledger ( PubKeyHash ) 
import qualified PlutusTx.AssocMap as AssocMap
import           Auction.Status


type BiddersMap = AssocMap.Map PubKeyHash Status
