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
import           Data.Aeson (encode)
import Plutus.Contracts.DropspotDMTDistro
import Ledger (PaymentPubKeyHash(PaymentPubKeyHash))

datum :: DistroDatum
datum = DistroDatum {
  distroTo = PaymentPubKeyHash "d006eb7783e8c93160b2bab287bc8a6f069e9e690cd82bc0b52a8c31", 
  quantity = 42
}

redeemer :: DistroAction
redeemer = Redeem

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
      dsWalletAddress = stringToBuiltinByteString (if nargs > 0 then read (args!!0) else "")
      mDmtPolicy       = stringToBuiltinByteString (if nargs > 1 then read (args!!1)  else "")
      mDmtToken        = stringToBuiltinByteString (if nargs > 2 then read (args!!2) else "")
      scriptname      = if nargs > 3 then args!!3 else  "result.plutus"

  putStrLn $ printf " { dsWalletAddress: %s, dmtPolicy: %s, dmtToken: %s  } " (show dsWalletAddress) (show mDmtPolicy) (show mDmtToken)

  let contractInfo = ContractInfo {
    dsAccount = "68ddc8867360a897f3840652c1352db1a6c7e1ca82e685e47032b36d",
    dmtPolicy = CurrencySymbol mDmtPolicy,
    dmtToken  = TokenName mDmtToken
  }

  putStrLn $ "Writing output to: " ++ scriptname
  writePlutusScript 42 scriptname (distroDMT contractInfo) (distroDMTSBS contractInfo)


  print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData datum))
  print $ "Redeemer value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData redeemer))

  putStrLn $ "Val Hash: " ++ show (valHash contractInfo)
  putStrLn $ "Script Address: " ++ show (scrAddress contractInfo)



writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
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