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

import           Plutus.Contracts.DropspotClaim

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


main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
      scriptname  = if nargs > 0 then args!!0 else  "result.plutus"
      datum = stringToBuiltinByteString "hello"
      redeemer = stringToBuiltinByteString "world"

  print $ "Datum value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData datum))
  print $ "Redeemer value: " <> encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData redeemer))


  writePlutusScript scriptname claimSerialised claimSBS


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