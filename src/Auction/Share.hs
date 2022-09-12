{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Auction.Share
    ( auctionDatum
    , auctionedTokenValue
    , minLovelace
    , notNull
    ) 
    where

import           Ledger 
import           Ledger.Value as Value 
import qualified PlutusTx
import           PlutusTx.Prelude 

import           Auction.Types 


{-# INLINABLE notNull #-}
notNull :: Foldable f => f a -> Bool
notNull = not . null


{-# INLINABLE auctionDatum #-}
auctionDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AuctionDatum
auctionDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d


auctionedTokenValue :: AssetClass -> Value
auctionedTokenValue x = Value.assetClassValue x 1


minLovelace :: Integer
minLovelace = 2000000


