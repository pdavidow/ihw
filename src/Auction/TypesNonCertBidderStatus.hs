module Auction.TypesNonCertBidderStatus
    ( AlreadyApproveds(..)
    , NotRegistereds(..)
    ) 
    where

import Ledger ( PubKeyHash ) 


newtype AlreadyApproveds = AlreadyApproveds [PubKeyHash] 
    deriving Show


newtype NotRegistereds = NotRegistereds [PubKeyHash] 
    deriving Show