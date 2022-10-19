
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}

module Plutus.Contracts.DropspotClaim where

import qualified PlutusTx
import PlutusTx.Code (getCovIdx)
import PlutusTx.Coverage (CoverageIndex)
import PlutusTx.Prelude hiding (pure, (<$>))
import qualified Prelude as Haskell
import Ledger (ScriptContext)
import qualified PlutusTx.Builtins   as Builtins

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Scripts as LS
import qualified Ledger.Typed.Scripts.Validators as SV
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Ledger hiding (singleton)
import Codec.Serialise

import qualified Plutus.Contract.Constraints as Constraints
import qualified Prelude as Haskell
import Ledger.Value as Value
import Ledger hiding (singleton)
import Data.Map as Map
import Data.Text (Text, pack)
import Data.Void (Void)
import Playground.Contract
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import Ledger.Constraints.TxConstraints
import Plutus.Contract.Request (awaitTxConfirmed, submitTxConstraintsWith)
import Text.Printf (printf)
import Ledger.Address
import Ledger.Ada as Ada

import Codec.Serialise
import Cardano.Api (writeFileTextEnvelope, displayError)

{-# INLINABLE mkValidator #-}
mkValidator ::  Builtins.BuiltinData -> Builtins.BuiltinData -> Builtins.BuiltinData -> ()
mkValidator hash clear _ = 
  if correct then () else (error ())
  where
    correct = isCorrect (PlutusTx.unsafeFromBuiltinData hash) (PlutusTx.unsafeFromBuiltinData clear)

{-# INLINABLE isCorrect #-}
isCorrect :: Builtins.BuiltinByteString -> Builtins.BuiltinByteString -> Bool
isCorrect hash' clear' = hash' == encodeHex (Builtins.sha2_256 clear')


-- Convert from a byte string to its hex (base16) representation. Example: [2, 14, 255] => "020eff"
{-# INLINEABLE encodeHex #-}
encodeHex :: BuiltinByteString -> BuiltinByteString
encodeHex input = go 0
  where
    len = lengthOfByteString input
    go :: Integer -> BuiltinByteString
    go i
      | i == len = emptyByteString
      | otherwise =
        consByteString (toChar $ byte `quotient` 16) $
          consByteString (toChar $ byte `remainder` 16) (go $ i + 1)
      where
        byte = indexByteString input i

        toChar :: Integer -> Integer
        toChar x
          -- 48 is ASCII code for '0'
          | x < 10 = x + 48
          -- 97 is ASCII code for 'a'
          -- x - 10 + 97 = x + 87
          | otherwise = x + 87


data Typed

instance Scripts.ValidatorTypes Typed where
  type DatumType Typed = Builtins.BuiltinByteString
  type RedeemerType Typed = Builtins.BuiltinByteString

dsClaimValidator :: LS.Validator
dsClaimValidator = LS.mkValidatorScript $$(PlutusTx.compile [||mkValidator||])


dsClaimScript :: LS.Script
dsClaimScript = LS.unValidatorScript dsClaimValidator

claimSBS :: SBS.ShortByteString
claimSBS = SBS.toShort . LBS.toStrict $ serialise dsClaimValidator

claimSerialised :: PlutusScript PlutusScriptV2
claimSerialised = PlutusScriptSerialised claimSBS

writeDropspotClaimScriptV2 :: IO ()
writeDropspotClaimScriptV2 = do 
  result <- writeFileTextEnvelope "ds-claim-v2.1.plutus" Nothing claimSerialised
  case result of
    Left err -> Haskell.print $ displayError err
    Right () -> return ()

