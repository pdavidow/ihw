-- {-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Duration 
    ( Duration(..)
    , plus
    , times
    , toMicroSec
    , toMilliSec
    , toPOSIX
    )
        where
               
import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

import           Ledger (POSIXTime(..)) 
import           Lib.NaturalNumber.NatGE0 (NatGE0, isZero, unNatGE0)
import qualified Lib.NaturalNumber.NatGE1 as N1 
import           Lib.NaturalNumber.NatGE1 (NatGE1)
 

data Duration
    = Months NatGE1
    | Months6
    | Months3
    | Month
    | Weeks NatGE1  
    | Week
    | Days NatGE1    
    | Day
    | Hours NatGE1    
    | Hour
    | Minutes NatGE1    
    | Minute
    | Seconds NatGE1
    | Second
    | None        
        deriving (Show, Generic)    
        deriving anyclass (ToJSON, FromJSON)    

instance Eq Duration where
    x == y = toPOSIX x == toPOSIX y

instance Ord Duration where
    x <= y = toPOSIX x <= toPOSIX y


{-# INLINABLE plus #-}
plus :: Duration -> Duration -> Duration
plus None None = None
plus x None = x
plus None y = y
plus x y = 
    let
        numer = getPOSIXTime $ toPOSIX x + toPOSIX y
        denom = getPOSIXTime $ toPOSIX Second
        (result, _) = divMod numer denom -- mod is zero by definition
    in
        Seconds $ N1.mkOk result


{-# INLINABLE times #-}
times :: NatGE0 -> Duration -> Duration
times n dur =
    if isZero n then 
        None
    else let x = N1.mkOk $ unNatGE0 n in 
        case dur of
            (Seconds y) -> Seconds $ x `N1.times` y
            (Minutes y) -> Minutes $ x `N1.times` y
            (Hours y)   -> Hours   $ x `N1.times` y
            (Days y)    -> Days    $ x `N1.times` y
            (Weeks y)   -> Weeks   $ x `N1.times` y
            (Months y)  -> Months  $ x `N1.times` y
            None        -> None
            Second      -> times n (Seconds N1.nat1)
            Minute      -> times n (Minutes N1.nat1)
            Hour        -> times n (Hours   N1.nat1)
            Day         -> times n (Days    N1.nat1)
            Week        -> times n (Weeks   N1.nat1)
            Month       -> times n (Months  N1.nat1)
            Months3     -> times n (Months  N1.nat3)
            Months6     -> times n (Months  N1.nat6)


{-# INLINABLE toPOSIX #-}
toPOSIX :: Duration -> POSIXTime
toPOSIX None        = POSIXTime 0
toPOSIX Second      = POSIXTime 1000
toPOSIX Minute      = toPOSIX $ Seconds $ N1.mkOk 60
toPOSIX Hour        = toPOSIX $ Minutes $ N1.mkOk 60
toPOSIX Day         = toPOSIX $ Hours   $ N1.mkOk 24
toPOSIX Week        = toPOSIX $ Days    $ N1.mkOk 7
toPOSIX Month       = toPOSIX $ Days    $ N1.mkOk 30
toPOSIX Months3     = toPOSIX $ Months  $ N1.mkOk 3
toPOSIX Months6     = toPOSIX $ Months  $ N1.mkOk 6
toPOSIX (Seconds n) = POSIXTime (N1.unNatGE1 n * getPOSIXTime (toPOSIX Second))
toPOSIX (Minutes n) = POSIXTime (N1.unNatGE1 n * getPOSIXTime (toPOSIX Minute))
toPOSIX (Hours   n) = POSIXTime (N1.unNatGE1 n * getPOSIXTime (toPOSIX Hour))
toPOSIX (Days    n) = POSIXTime (N1.unNatGE1 n * getPOSIXTime (toPOSIX Day))
toPOSIX (Weeks   n) = POSIXTime (N1.unNatGE1 n * getPOSIXTime (toPOSIX Week))   
toPOSIX (Months  n) = POSIXTime (N1.unNatGE1 n * getPOSIXTime (toPOSIX Month))    
  

{-# INLINABLE toMilliSec #-}
toMilliSec :: Duration -> Integer 
toMilliSec = getPOSIXTime . toPOSIX


{-# INLINABLE toMicroSec #-}
toMicroSec :: Duration -> Integer 
toMicroSec = (* 1000) . toMilliSec