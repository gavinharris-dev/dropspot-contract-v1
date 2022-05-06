module Play (main) where

import Data.Default (def)
import GHC.Base (when)
import Ledger.Interval
import Ledger.TimeSlot
import Plutus.Contract (logInfo)
import qualified PlutusTx.Prelude

main :: IO ()
main = do
  let currentSlot = slotToPOSIXTimeRange def 10
      start = slotToBeginPOSIXTime def 4

  print start
  putStrLn ("Result is " ++ if (from start `contains` currentSlot) then "True" else "False")
 
--  Plutus.V1.Ledger.Time.POSIXTimeRange
 -- POSIXTimeRange