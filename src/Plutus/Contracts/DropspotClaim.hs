
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

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
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

{-# INLINABLE mkValidator #-}
mkValidator ::  Builtins.BuiltinByteString -> Builtins.BuiltinByteString -> ScriptContext -> Bool
mkValidator hash clear _ = isCorrect hash clear

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

typedValidator :: Scripts.TypedValidator Typed
typedValidator =
  Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [||mkValidator||])
      $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Builtins.BuiltinByteString @Builtins.BuiltinByteString

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

valAddressHash :: Ledger.Address
valAddressHash = Scripts.validatorAddress typedValidator

claimSBS :: SBS.ShortByteString
claimSBS = SBS.toShort . LBS.toStrict $ serialise validator

claimSerialised :: PlutusScript PlutusScriptV1
claimSerialised = PlutusScriptSerialised claimSBS

claimScriptSize :: Integer
claimScriptSize = LS.scriptSize $ LS.getValidator $ SV.validatorScript typedValidator

data Params = Params
  { policy :: !CurrencySymbol,
    token :: !TokenName,
    password :: !BuiltinByteString
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

type RedemptionSchema =
        Endpoint "list" Params
    .\/ Endpoint "redeem" Params

list :: forall w s. Params -> Contract w s Text ()
list listParams = do
    let 
      datum = Datum $ PlutusTx.toBuiltinData $ encodeHex (Builtins.sha2_256 (password listParams))
      v = Value.singleton (policy listParams) (token listParams) 1 Haskell.<> Ada.lovelaceValueOf 3_000_000
      tx = Constraints.mustPayToOtherScript valHash datum v
      
    ledgerTx <- submitTx tx

    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Haskell.String "listed token with password"

redeem :: forall w s. Params -> Contract w s Text ()
redeem redeemParams = do
    (oref, o, d) <- findOffer (policy redeemParams) (token redeemParams)
    
    logInfo @Haskell.String $ printf "found offer utxo with datum %s" (Haskell.show d)
    
    let lookups = Constraints.unspentOutputs (Map.singleton oref o) Haskell.<>
                  Constraints.otherScript validator
        redeemer = Redeemer $ PlutusTx.toBuiltinData (password redeemParams)
        tx = Constraints.mustSpendScriptOutput oref redeemer

    myTxn <- mkTxConstraints lookups tx

    logInfo @Haskell.String $ printf ">>>> TXN: %s" (Haskell.show myTxn)

    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @Haskell.String $ "Claimed Token"

findOffer :: CurrencySymbol -> TokenName -> Contract w s Text (TxOutRef, ChainIndexTxOut, Builtins.BuiltinData)
findOffer cs tn = do
  utxos <- utxosAt $ scriptHashAddress valHash
  let xs =
        [ (oref, o)
          | (oref, o) <- Map.toList utxos,
            Value.valueOf (_ciTxOutValue o) cs tn == 1
        ]
  case xs of
    [(oref, o)] -> case _ciTxOutDatum o of
      Left _ -> throwError "datum has wrong type"
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> throwError "Wrong Datum Type"
        Just d -> return (oref, o, d)
    _ -> throwError "Cannot find Offer UTxO"


testPolicy :: CurrencySymbol
testPolicy = CurrencySymbol "fff"

testAssetName :: TokenName
testAssetName = TokenName "T"

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash (unCurrencySymbol testPolicy)) "Token" (TokenName (unTokenName testAssetName) :| [])


endpoints :: Contract () RedemptionSchema Text ()
endpoints = awaitPromise (list' `select` redeem') >> endpoints
  where
    list' = endpoint @"list" list
    redeem' = endpoint @"redeem" redeem

mkSchemaDefinitions ''RedemptionSchema

$(mkKnownCurrencies ['myToken])