module Auction.Synonyms
    ( BiddersMap
    ) 
    where

import           Ledger ( PubKeyHash ) 
-- import qualified PlutusTx.AssocMap as AssocMap
-- import           Auction.Status ( Status )
import           Auction.Types ( ApproveParams(..), BidParams(..), CloseParams(..), RegisterParams(..), StartParams(..) )


-- type BiddersMap = AssocMap.Map PubKeyHash Status

type AuctionSchema =
        Endpoint "start"    StartParams
    .\/ Endpoint "bid"      BidParams
    .\/ Endpoint "close"    CloseParams
    .\/ Endpoint "register" RegisterParams
    .\/ Endpoint "approve"  ApproveParams
