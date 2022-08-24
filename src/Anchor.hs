{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Anchor
    ( Anchor(..)
    , AnchorGraveyard(..)    
    , anchorTokenName
    , anchorAsset
    , anchorValue
    , isAnchoredBy
    , isNotAnchoredBy
    ) 
    where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import qualified Prelude as H--askell     
import           Ledger
                    ( ownHash,
                    valueLockedBy,
                    ScriptContext,
                    TxInfo,
                    AssetClass,
                    CurrencySymbol,
                    PubKeyHash, 
                    TokenName,
                    Value )
import           Ledger.Value
                    ( AssetClass(AssetClass),
                    CurrencySymbol(CurrencySymbol),
                    TokenName(TokenName),
                    assetClassValue,
                    assetClassValueOf )
import           PlutusTx.Prelude ( Bool, Eq(..), ($), not, traceIfFalse ) 
import qualified PlutusTx
import           Schema (ToSchema)


newtype Anchor = Anchor {unAnchor :: CurrencySymbol}
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)    
    deriving newtype (Eq)

PlutusTx.makeIsDataIndexed ''Anchor [('Anchor, 0)]
PlutusTx.makeLift ''Anchor 


newtype AnchorGraveyard  = AnchorGraveyard {unAnchorGraveyard :: PubKeyHash}
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)    
    deriving newtype (Eq)

PlutusTx.makeIsDataIndexed ''AnchorGraveyard [('AnchorGraveyard, 0)]
PlutusTx.makeLift ''AnchorGraveyard 


{-# INLINABLE anchorTokenName #-}
anchorTokenName :: TokenName
anchorTokenName = TokenName "--------------*|===}" -- arbitrary


{-# INLINABLE anchorAsset #-}
anchorAsset :: Anchor -> AssetClass
anchorAsset x = AssetClass (unAnchor x, anchorTokenName)


{-# INLINABLE anchorValue #-}
anchorValue :: Anchor -> Ledger.Value 
anchorValue x = assetClassValue (anchorAsset x) 1


{-# INLINABLE isAnchoredBy #-}
isAnchoredBy :: Anchor -> TxInfo -> ScriptContext -> Bool
isAnchoredBy a info ctx = {- traceIfFalse "anchor-token not locked" $ -} assetClassValueOf (valueLockedBy info $ ownHash ctx) (anchorAsset a) == 1


{-# INLINABLE isNotAnchoredBy #-}
isNotAnchoredBy :: Anchor -> TxInfo -> ScriptContext -> Bool
isNotAnchoredBy a info ctx = {- traceIfFalse "anchor-token locked" $  -} not $ isAnchoredBy a info ctx