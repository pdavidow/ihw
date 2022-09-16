{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Auction.Share
    ( auctionDatum
    , auctionedTokenValue
    , isSeller
    , minLovelace
    , notNull
    ) 
    where

import           Ledger ( txOutDatum, PubKeyHash, Datum(..), DatumHash, TxOut, AssetClass, Value ) 
import           Ledger.Value as Value ( assetClassValue ) 
import qualified PlutusTx
import           PlutusTx.Prelude ( Bool, Integer, Maybe, Eq((==)), (.), not, null, Foldable ) 

import           Auction.Types ( Seller(..), AuctionDatum ) 


{-# INLINABLE notNull #-}
notNull :: Foldable f => f a -> Bool
notNull = not . null


{-# INLINABLE auctionDatum #-}
auctionDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AuctionDatum
auctionDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d


{-# INLINABLE auctionedTokenValue #-}
auctionedTokenValue :: AssetClass -> Value
auctionedTokenValue x = Value.assetClassValue x 1


{-# INLINABLE minLovelace #-}
minLovelace :: Integer
minLovelace = 2000000


{-# INLINABLE isSeller #-}
isSeller :: PubKeyHash -> Seller -> Bool
isSeller pkh x = unSeller x == pkh   