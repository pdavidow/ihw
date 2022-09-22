{-# LANGUAGE NoImplicitPrelude #-}


module Auction.Share
    ( auctionedTokenValue
    , isSeller
    , notNull
    ) 
    where

import           Ledger ( PubKeyHash, AssetClass, Value ) 
import           Ledger.Value as Value ( assetClassValue ) 
import           PlutusTx.Prelude ( Bool, Eq((==)), (.), not, null, Foldable ) 

import           Auction.Types ( Seller(..) ) 


{-# INLINABLE notNull #-}
notNull :: Foldable f => f a -> Bool
notNull = not . null


{-# INLINABLE auctionedTokenValue #-}
auctionedTokenValue :: AssetClass -> Value
auctionedTokenValue x = Value.assetClassValue x 1


{-# INLINABLE isSeller #-}
isSeller :: PubKeyHash -> Seller -> Bool
isSeller pkh x = unSeller x == pkh   