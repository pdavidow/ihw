{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( adaAssetClass
    , auctionFee
    , lovelaceEquity
    , lovelaceNEPosValue
    , companyPkh  
    , pubKeyHashForWalletNum
    , wallet    
    , walletPubKeyHash
    ) where

import           Ledger ( PubKeyHash ) 
import           Plutus.V1.Ledger.Ada ( adaSymbol, adaToken )
import           Plutus.V1.Ledger.Value ( AssetClass, assetClass ) 
import           Wallet.Emulator.Wallet (Wallet, knownWallet, walletPubKeyHash)

import           CompanyFee ( CompanyFee(..) )
import           Equity as E ( Equity, singleton )
import           Lib.NaturalNumber.NatGE1 as N1 ( NatGE1 )
import qualified Lib.NEPosValue as NEPV (singleton) 
import           Lib.NEPosValue (NEPosValue)
import qualified PercentTimesTen as PctX10


wallet :: Integer -> Wallet
wallet = knownWallet


pubKeyHashForWalletNum :: Integer -> PubKeyHash
pubKeyHashForWalletNum = walletPubKeyHash . knownWallet


companyPkh :: PubKeyHash
companyPkh = walletPubKeyHash $ wallet 1


lovelaceEquity :: NatGE1 -> Equity
lovelaceEquity = E.singleton adaAssetClass 


lovelaceNEPosValue :: NatGE1 -> NEPosValue
lovelaceNEPosValue = NEPV.singleton adaSymbol adaToken 


adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken


-- todo 
auctionFee :: CompanyFee
auctionFee = CompanyFee $ PctX10.mkOk 45