module Play (main) where

import           Text.Printf (printf)
import           Plutus.V1.Ledger.Api
import PlutusTx.Builtins.Class
import Cardano.Api (scriptDataToJson, toCBOR)
import Cardano.Api.Byron (ScriptDataJsonSchema(ScriptDataJsonDetailedSchema))
import Cardano.Api.Shelley (fromPlutusData)
import  Data.Aeson (encode)
import Ledger (PaymentPubKeyHash(PaymentPubKeyHash))
import qualified Plutus.V1.Ledger.Api as Plutus

datumToCBOR :: ()
datumToCBOR = do
  let datum = stringToBuiltinByteString "Test"

  print $ "Datum value: " <> encode (toCBOR (fromPlutusData (Plutus.toData datum)))


-- main :: IO ()
-- main = do
--   let currentSlot = slotToPOSIXTimeRange def 10
--       start = slotToBeginPOSIXTime def 4

--   print start
--   putStrLn ("Result is " ++ if (from start `contains` currentSlot) then "True" else "False")
 
-- --  Plutus.V1.Ledger.Time.POSIXTimeRange
--  -- POSIXTimeRange