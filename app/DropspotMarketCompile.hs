{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

import           Plutus.Contracts.DropspotMarket

import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Data.ByteString.Short as SBS
import           Text.Printf (printf)
import           Plutus.V1.Ledger.Api
import PlutusTx.Builtins.Class
import Cardano.Api (scriptDataToJson)
import Cardano.Api.Byron (ScriptDataJsonSchema(ScriptDataJsonDetailedSchema))
import Cardano.Api.Shelley (fromPlutusData)
import  Data.Aeson (encode)
import Ledger (PaymentPubKeyHash(PaymentPubKeyHash))


datum :: MarketDatum
datum = MarketDatum
        {
          royalties = [
            DisbursementItem {
              wallet = PaymentPubKeyHash "89400023E22ECEA2CA12283BC4C65B78723905BE3306D1716E0391FD",
              percent = 5000
            },
            DisbursementItem {
              wallet = PaymentPubKeyHash "98400023E22ECEA2CA12283BC4C65B78723905BE3306D1716E0391FD",
              percent = 3000
            }
          ],
          disburements = [
            DisbursementItem {
              wallet = PaymentPubKeyHash "89400023E22ECEA2CA12283BC4C65B78723905BE3306D1716E0391FD",
              percent = 0}
          ],
          tradeOwner = PaymentPubKeyHash "89400023E22ECEA2CA12283BC4C65B78723905BE3306D1716E0391FD",
          amount = 100000000,
          policy = CurrencySymbol "103F42252C7EFC783E99035F39714F749A11C5E7408EB459630DABA9",
          token = TokenName "416E6F746865724F6E65546F6B656E36",
          startDate = 1596059092000
        }
    
-- testDatum :: MarketDatum
-- testDatum = MarketDatum {
--   royalties = [],
--   disburements = [],
--   tradeOwner = PaymentPubKeyHash "d006eb7783e8c93160b2bab287bc8a6f069e9e690cd82bc0b52a8c31",
--   amount = 70000002,
--   policy = CurrencySymbol "777be88df242bd81b95d8947d099086abf1896a4d693be4a80388ee9",
--   token = TokenName "74444d54",
--   startDate = 1652935982942
-- }

-- printDatum :: IO ()
-- printDatum = do
--   print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData testDatum))

redeemer :: MarketAction
redeemer = Relist

-- main :: IO ()
-- main = do 
--   printDatum

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
      argMarketPlaceOwner = stringToBuiltinByteString (if nargs > 0 then read (args!!0) else "")
      argMarketPlacePCT   = if nargs > 1 then read (args!!1)  else 400
      scriptname      = if nargs > 2 then args!!2 else  "result.plutus"

  putStrLn $ printf " { dsWalletAddress: %s, Percent: %s } " (show argMarketPlaceOwner) (show argMarketPlacePCT)

  putStrLn $ "Writing output to: " ++ scriptname

  -- let contractInfo = ContractInfo {
  --   marketPlaceOwner = PaymentPubKeyHash $ PubKeyHash argMarketPlaceOwner,
  --   marketPlacePCT = argMarketPlacePCT
  -- }

  -- let humanPart = Bech32.humanReadablePartFromText $ T.pack "test_addr";
  --     dataPart = Bech32.dataPartFromBytes $ valHash contractInfo 

  writePlutusScript scriptname (dropspotMarketlised contractInfo) (dropspotMarketSBS contractInfo)
  
  putStrLn $ "Script Address: " ++ show (scrAddress contractInfo)

  print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData datum))
  print $ "Redeemer value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData redeemer))


writePlutusScript :: FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber 42)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()