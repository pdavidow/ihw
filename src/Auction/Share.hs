{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Auction.Share
    ( auctionDatum
    , auctionedTokenValue
    , minBid
    , minLovelace
    ) 
    where

import           Ledger ( Datum(Datum), DatumHash, TxOut, txOutDatum ) 
import           Ledger.Value as Value ( Value, singleton )
import qualified PlutusTx
import           PlutusTx.Prelude ( Integer, Maybe(..), AdditiveSemigroup((+))) 

import           Auction.Types ( Auction(..), Bid(..), AuctionDatum(..) )
     

{-# INLINABLE minBid #-}
minBid :: AuctionDatum -> Integer
minBid AuctionDatum{..} = case adHighestBid of
    Nothing      -> aMinBid adAuction
    Just Bid{..} -> bBid + 1


{-# INLINABLE auctionDatum #-}
auctionDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AuctionDatum
auctionDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d


auctionedTokenValue :: Auction -> Value
auctionedTokenValue x = Value.singleton (aCurrency x) (aToken x) 1


minLovelace :: Integer
minLovelace = 2000000


