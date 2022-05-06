
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

module Plutus.Contracts.DropspotDMTDistro where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import Control.Monad hiding (fmap)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Map as Map
import Data.Text (Text, pack)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Address
import qualified Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Playground.Contract
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import qualified Plutus.Contract.Constraints as Constraints
import Plutus.Contract.Request (awaitTxConfirmed)
import Plutus.Contract.Types (AsContractError)
import qualified Plutus.V1.Ledger.Ada as Ada
import Plutus.V1.Ledger.Interval (from)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified PlutusTx.Foldable as Foldable
import Text.Printf (printf)
import qualified Prelude as Haskell
import Data.Aeson (Value(Bool))

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy  as LBS
import Cardano.Binary

data ContractInfo = ContractInfo {
  dmtPolicy :: CurrencySymbol,
  dmtToken  :: TokenName,
  dsAccount :: PubKeyHash
}

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 0)]
PlutusTx.makeLift ''ContractInfo

data DistroDatum = DistroDatum {
  distroTo :: PaymentPubKeyHash,
  quantity :: Integer
}
  deriving (Generic, ToJSON, FromJSON, Haskell.Show, FromCBOR, ToCBOR)

PlutusTx.makeIsDataIndexed ''DistroDatum [('DistroDatum, 0)]
PlutusTx.makeLift ''DistroDatum

data DistroAction = Redeem | Cancel
  deriving (Generic, ToJSON, FromJSON, FromCBOR, ToCBOR)

PlutusTx.makeIsDataIndexed ''DistroAction [('Redeem, 0), ('Cancel, 1)]
PlutusTx.makeLift ''DistroAction

{-# INLINEABLE minLovelace #-}
minLovelace :: Integer
minLovelace = 2_000_000

{-# INLINEABLE mkValidator #-}
mkValidator :: ContractInfo -> DistroDatum -> DistroAction -> ScriptContext -> Bool
mkValidator ci datum action context =
  case action of

    Redeem -> traceIfFalse "REDEEM: Are not distro'ing the tokens" userGetsTokens &&
              traceIfFalse "REDEEM: Must be signed by the user to whom the token is distro'd to" signedByUser &&
              traceIfFalse "REDEEM: Dropspot should get their UTXO Minimum back" dsGetsTheirUTXOMinBack

    Cancel -> traceIfFalse "CANCEL: DS should get the tokens back on Cancel" dsGetsTheTokensBack &&
              traceIfFalse "CANCEL: Dropspot should get their UTXO Minimum back" dsGetsTheirUTXOMinBack

    where

      txInfo :: TxInfo
      txInfo = scriptContextTxInfo context

      signer :: PubKeyHash
      signer = case txInfoSignatories txInfo of
        [pkh] -> pkh

      signedByUser :: Bool
      signedByUser = txSignedBy txInfo $ unPaymentPubKeyHash $ distroTo datum

      userGetsTokens :: Bool
      userGetsTokens = True
        -- Value.valueOf valuePaidToUser (dmtPolicy ci) (dmtToken ci) == quantity datum
      
      valuePaidToUser :: Value.Value
      valuePaidToUser = valuePaidTo txInfo (unPaymentPubKeyHash $ distroTo datum)

      dsGetsTheirUTXOMinBack :: Bool
      dsGetsTheirUTXOMinBack = Ada.fromValue (valuePaidTo txInfo (dsAccount ci)) >= Ada.lovelaceOf minLovelace

      dsGetsTheTokensBack :: Bool
      dsGetsTheTokensBack = valueOf (valuePaidTo txInfo (dsAccount ci)) (dmtPolicy ci) (dmtToken ci) >= quantity datum

data Typed

instance Scripts.ValidatorTypes Typed where
  type DatumType Typed = DistroDatum
  type RedeemerType Typed = DistroAction

typedValidator :: ContractInfo ->  Scripts.TypedValidator Typed
typedValidator ci =
  Scripts.mkTypedValidator @Typed
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode ci)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @DistroDatum @DistroAction

validator :: ContractInfo -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: ContractInfo -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: ContractInfo -> Ledger.Address
scrAddress = scriptAddress . validator

distroDMTSBS :: ContractInfo -> SBS.ShortByteString
distroDMTSBS ci = SBS.toShort . LBS.toStrict $ serialise (validator ci)

distroDMT :: ContractInfo -> PlutusScript PlutusScriptV1
distroDMT ci = PlutusScriptSerialised (distroDMTSBS ci)


-- Off Chain

type DMTClaimSchema =
  Endpoint "offer" OfferParams
    .\/ Endpoint "redeem" ()
   -- .\/ Endpoint "cancel" ()

data OfferParams = OfferParams {
  offerTo :: !PaymentPubKeyHash ,
  offerQuantity :: !Integer
}
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

testContractInfo :: ContractInfo
testContractInfo =
  ContractInfo
    { dsAccount = "9e09ad3c6d542b0c53b36793de1c26dfa15f1c4005fd000726792c36", -- TEST Wallet
      dmtPolicy = CurrencySymbol "777be88df242bd81b95d8947d099086abf1896a4d693be4a80388ee9",
      dmtToken  = TokenName "tDMT"
    }


offer :: AsContractError e => OfferParams -> Contract w s e ()
offer OfferParams {..} = do
  let d = DistroDatum { 
    distroTo = offerTo, 
    quantity = offerQuantity 
  }

      v = Value.singleton (dmtPolicy testContractInfo) (dmtToken testContractInfo) offerQuantity
            Haskell.<> Ada.lovelaceValueOf minLovelace

      tx = Constraints.mustPayToTheScript d v

  ledgerTx <- submitTxConstraints (typedValidator testContractInfo) tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @Haskell.String $ printf "Transaction %s" (Haskell.show ledgerTx)

findOffer :: PubKeyHash -> Contract w s Text (TxOutRef, ChainIndexTxOut, DistroDatum)
findOffer pkh = do
  utxos <- utxosAt $ scriptHashAddress (valHash testContractInfo)
  logInfo @Haskell.String $ printf "UTXOs: %s" (Haskell.show utxos)
  let xs =
        [ (oref, o)
          | (oref, o) <- Map.toList utxos
        ]

  logInfo @Haskell.String $ printf "Found %d offers [%s]" (length xs) (Haskell.show xs)
  case xs of
    [(oref, o)] -> case _ciTxOutDatum o of
      Left _ -> throwError "datum has wrong type"
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> throwError "Wrong Datum Type"
        Just d@DistroDatum {..}
          | True -> return (oref, o, d)
          | otherwise -> throwError "Cannot find offer"
    _ -> throwError "Cannot find Offer UTxO"

redeem :: forall w s. () -> Contract w s Text ()
redeem _ = do

  pkh <- ownPaymentPubKeyHash

  (oref, o, d@DistroDatum {..}) <- findOffer (unPaymentPubKeyHash pkh)
  let r = Redeemer $ PlutusTx.toBuiltinData Redeem

      lookups =
        Constraints.typedValidatorLookups (typedValidator testContractInfo)
          Haskell.<> Constraints.ownPaymentPubKeyHash pkh
          Haskell.<> Constraints.otherScript (validator testContractInfo)
          Haskell.<> Constraints.unspentOutputs (Map.singleton oref o)

      tx = Constraints.mustPayToPubKey (PaymentPubKeyHash $ dsAccount testContractInfo) (Ada.lovelaceValueOf minLovelace)
            Haskell.<> Constraints.mustSpendScriptOutput oref r

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @Haskell.String $ printf "TXN: %d" $ Haskell.show ledgerTx




mkSchemaDefinitions ''DMTClaimSchema

endpoints :: Contract () DMTClaimSchema Text ()
endpoints = awaitPromise (offer' `select` redeem') >> endpoints
  where
    offer' = endpoint @"offer" offer
    redeem' = endpoint @"redeem" redeem

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "tDMT" :| [])

$(mkKnownCurrencies ['myToken])


-- Method used to output example Datum. Used for Developing.
distroDatumToData :: PlutusTx.Data
distroDatumToData =
  PlutusTx.toData (
    DistroDatum {
      distroTo = PaymentPubKeyHash "d006eb7783e8c93160b2bab287bc8a6f069e9e690cd82bc0b52a8c31",
      quantity = 55
    }
  )

-- testDatum :: ()
-- testDatum = do
--   d <- B16.decode "d866820082581cd006eb7783e8c93160b2bab287bc8a6f069e9e690cd82bc0b52a8c311837"
--   -- DistroDatum {..} <- PlutusTx.fromBuiltinData d
--   logInfo @Haskell.String $ printf "Datum: %s" (Haskell.show d)