module Lib.Pkh
    ( pkhFromStr
    )
    where

import           Data.String (IsString(fromString))
import           Data.Text (Text, pack)

import           Ledger (PubKeyHash(..)) 
import           Ledger.Bytes (LedgerBytes(LedgerBytes), fromHex)


pkhFromStr :: String -> Either Text PubKeyHash   
pkhFromStr s =         
    case fromHex (fromString s) of 
        Right (LedgerBytes bytes) -> Right $ PubKeyHash bytes 
        Left msg -> Left $ pack ("Could not convert from hex to bytes: " <> msg)   