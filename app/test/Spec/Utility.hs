{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Spec.Utility
    ( slotsInMinutes
    , walletFundsChangeWithinNatGE1Tolerance
    ) where

 
import qualified Control.Foldl                         as L
import           Control.Lens                          (at, (^.))
import           Control.Monad                         (unless)
import           Control.Monad.Freer.Reader            (ask)
import           Data.Foldable                         (fold)
import           Control.Monad.Freer.Writer            (tell)
import           Data.Text.Prettyprint.Doc             (Pretty(pretty), Doc, viaShow, (<+>), vsep)
import           Data.Void                             (Void)
import qualified Ledger.Ada                            as Ada
import qualified PlutusTx.Prelude                      as P
import           Ledger.Value as V                     (Value, assetClassValue, leq, geq)
import           Plutus.Contract.Test                  (TracePredicate)
import           Plutus.Contract.Trace as X            (Wallet, InitialDistribution)
import           Wallet.Emulator.Folds                 (postMapM)
import qualified Wallet.Emulator.Folds                 as Folds
import           Utility                               (adaAssetClass) 


slotsInMinutes :: Integer -> Integer
slotsInMinutes n = 60 * n


-- todo https://github.com/input-output-hk/plutus/issues/4199

walletFundsChangeWithinNatGE1Tolerance :: Value -> Wallet -> Value -> TracePredicate
walletFundsChangeWithinNatGE1Tolerance positiveTolerance w dlt =
    flip postMapM (L.generalize $ (,) <$> Folds.walletFunds w <*> Folds.walletFees w) $ \(finalValue', fees) -> do
        dist <- ask @InitialDistribution
        let exact = False
            initialValue = fold (dist ^. at w)
            finalValue = finalValue' P.+ if exact then mempty else fees
            deltized = initialValue P.+ dlt
            result = (deltized `geq` finalValue) && (deltized `leq` (finalValue <> positiveTolerance))

        unless result $ do
            tell @(Doc Void) $ vsep $
                [ "Expected funds of" <+> pretty w <+> "to change by"
                , " " <+> viaShow dlt] ++
                (if exact then [] else ["  (excluding" <+> viaShow (Ada.getLovelace (Ada.fromValue fees)) <+> "lovelace in fees)" ]) ++
                if initialValue == finalValue
                then ["but they did not change"]
                else ["but they changed by", " " <+> viaShow (finalValue P.- initialValue)]
                
        pure result