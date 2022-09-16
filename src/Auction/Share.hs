{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Auction.Share
    ( auctionedTokenValue
    , isSeller
    , minLovelace
    , notNull
    ) 
    where

import           Ledger ( PubKeyHash, AssetClass, Value ) 
import           Ledger.Value as Value ( assetClassValue ) 
import           PlutusTx.Prelude ( Bool, Integer, Eq((==)), (.), not, null, Foldable ) 

import           Auction.Types ( Seller(..) ) 


{-# INLINABLE notNull #-}
notNull :: Foldable f => f a -> Bool
notNull = not . null


{-# INLINABLE auctionedTokenValue #-}
auctionedTokenValue :: AssetClass -> Value
auctionedTokenValue x = Value.assetClassValue x 1


{-# INLINABLE minLovelace #-}
minLovelace :: Integer
minLovelace = 2000000


{-# INLINABLE isSeller #-}
isSeller :: PubKeyHash -> Seller -> Bool
isSeller pkh x = unSeller x == pkh   