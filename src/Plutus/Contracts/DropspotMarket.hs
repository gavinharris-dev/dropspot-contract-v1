{-
  Dropspot Marketplace Plutus Smart Contract - v0.2

  Input Datum:
    marketPlaceOwner :: !PubKeyHash, -- Wallet address for Dropspot

    marketPlacePCT :: !Integer,
      Percentage to be paid to the market place owner (calculate this by taking ((1 / percentage) * 10)) - 0.4% would be ((1/(0.4/100)) * 10) = 2500.

    tradeOwner :: !PubKeyHash,       -- Owners Wallet Public Key hash
    amount :: !Integer,              -- Minimum Trade Amount for Token
    policy :: !BuiltinByteString,    -- Policy of NFT that we are selling
    token :: !BuiltinByteString      -- NFT that we are selling

-}
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

module Plutus.Contracts.DropspotMarket (dropspotMarketSBS, dropspotMarketlised, marketDatumToData, marketActionToData, valHash, scrAddress) where

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
import Plutus.Contract.Request
import Plutus.Contract.Request (awaitTxConfirmed)
import Plutus.Contract.Types (AsContractError)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import qualified Prelude as Haskell

data ContractInfo = ContractInfo
  { marketPlaceOwner :: !PubKeyHash, -- Wallet address for Dropspot
    marketPlacePCT :: !Integer -- Percentage to be paid to the market place owner
  }

contractInfo :: ContractInfo
contractInfo =
  ContractInfo
    { marketPlaceOwner = "9e09ad3c6d542b0c53b36793de1c26dfa15f1c4005fd000726792c36", -- TEST Wallet
      marketPlacePCT = 400 -- 2.5%
    }

data MarketDatum = MarketDatum
  { royaltyOwner :: !PaymentPubKeyHash, -- Wallet address for Dropspot
    royaltyPCT :: !Integer, -- Percentage to be paid to the market place owner
    tradeOwner :: !PaymentPubKeyHash, -- Owners Wallet Public Key hash
    amount :: !Integer, -- Minimum Trade Amount for Token
    policy :: !CurrencySymbol, -- Policy of NFT that we are selling
    token :: !TokenName, -- NFT that we are selling
    startDate :: !POSIXTime -- The NFT cannot be purchased before this Date
  }
  deriving (Generic, ToJSON, FromJSON, Haskell.Show)

data MarketAction = Buy | Cancel
  deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 0)]
PlutusTx.makeLift ''ContractInfo

PlutusTx.makeIsDataIndexed ''MarketDatum [('MarketDatum, 0)]
PlutusTx.makeLift ''MarketDatum

PlutusTx.makeIsDataIndexed ''MarketAction [('Buy, 0), ('Cancel, 1)]
PlutusTx.makeLift ''MarketAction

-- On Chain Validator

{-# INLINEABLE minLovelace #-}
minLovelace :: Integer
minLovelace = 2000000

{-# INLINEABLE lovelacePercentage #-}
lovelacePercentage :: Integer -> Integer -> Integer
lovelacePercentage am p =
  if p > 0
    then if result < minLovelace then minLovelace else result
    else 0 -- Prevent Divide By Zero
  where
    result = (am * 10) `PlutusTx.Prelude.divide` p

{-# INLINEABLE mkValidator #-}
mkValidator :: ContractInfo -> MarketDatum -> MarketAction -> ScriptContext -> Bool
mkValidator ci mkDatum mkAction context = case mkAction of
  -- Buy action need to verify that:
  --   1. The Trade Owner is being paid at least the 'minimum' amount
  --   2. The Token is being xferd to the buyer
  Buy ->
    purchaseStartDatePassed
      && correctlySplit (marketPlaceOwner ci) (marketPlacePCT ci) (unPaymentPubKeyHash $ royaltyOwner mkDatum) (royaltyPCT mkDatum) (unPaymentPubKeyHash $ tradeOwner mkDatum) (txInfoFee txInfo) (amount mkDatum)
      && buyerGetsPaid (valuePaidTo txInfo signer) (policy mkDatum) (token mkDatum)
  -- Cancel action need to verify that:
  --   1. The Trade Owner should geti their NFT Back
  --   2. If / When we go to a Bid process the current Highest Big should be repaid.
  Cancel -> containsNFT (valuePaidTo txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum)) (policy mkDatum) (token mkDatum)
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo context

    -- We are assuming that the Wallet Signing the Transaction will be the recipient of the NFT
    signer :: PubKeyHash
    signer = case txInfoSignatories txInfo of
      [pkh] -> pkh

    -- tradeOwnerGetsPaid :: PubKeyHash -> Integer -> Bool
    -- tradeOwnerGetsPaid ownerAddress minAmount = traceIfFalse "Trade Owner does not get at least requested Price" $ Ada.fromValue (valuePaidTo txInfo ownerAddress) >= Ada.lovelaceOf minAmount

    buyerGetsPaid :: Value -> CurrencySymbol -> TokenName -> Bool
    buyerGetsPaid v policy asset = traceIfFalse "The Buyer is not going to get their NFT" $ containsNFT v policy asset

    containsNFT :: Value -> CurrencySymbol -> TokenName -> Bool
    containsNFT v policy asset = valueOf v policy asset >= 1

    correctlySplit :: PubKeyHash -> Integer -> PubKeyHash -> Integer -> PubKeyHash -> Integer -> Integer -> Bool
    correctlySplit dsWallet dsPct royaltyWallet royatyPct tradeWallet fees total =
      let dsAmount = lovelacePercentage total dsPct
          royaltyAmount = lovelacePercentage total royatyPct
       in traceIfFalse "Dropspot Marketplace should get its share" (Ada.fromValue (valuePaidTo txInfo dsWallet) >= Ada.lovelaceOf dsAmount)
            && traceIfFalse "The Roylaty Owner should get its share" (Ada.fromValue (valuePaidTo txInfo royaltyWallet) >= Ada.lovelaceOf royaltyAmount)
            && traceIfFalse "The seller should get the remaining Ada" (Ada.fromValue (valuePaidTo txInfo tradeWallet) >= Ada.lovelaceOf (total - dsAmount - royaltyAmount - fees))

    purchaseStartDatePassed :: Bool
    purchaseStartDatePassed = traceIfFalse "Asset is not yet for sale" True --FIXME: Why does this not work? (contains (from (startDate mkDatum)) (txInfoValidRange txInfo))

-- Compile Section

data Typed

instance Scripts.ValidatorTypes Typed where
  type DatumType Typed = MarketDatum
  type RedeemerType Typed = MarketAction

typedValidator :: Scripts.TypedValidator Typed
typedValidator =
  Scripts.mkTypedValidator @Typed
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @MarketDatum @MarketAction

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

dropspotMarketSBS :: SBS.ShortByteString
dropspotMarketSBS = SBS.toShort . LBS.toStrict $ serialise validator

dropspotMarketlised :: PlutusScript PlutusScriptV1
dropspotMarketlised = PlutusScriptSerialised dropspotMarketSBS

marketDatumToData :: PlutusTx.Data
marketDatumToData =
  PlutusTx.toData
    ( MarketDatum
        { royaltyOwner = PaymentPubKeyHash "89400023E22ECEA2CA12283BC4C65B78723905BE3306D1716E0391FD",
          royaltyPCT = 5000,
          tradeOwner = PaymentPubKeyHash "89400023E22ECEA2CA12283BC4C65B78723905BE3306D1716E0391FD", -- Owners Wallet Public Key hash
          amount = 100000000,
          policy = CurrencySymbol "103F42252C7EFC783E99035F39714F749A11C5E7408EB459630DABA9",
          token = TokenName "416E6F746865724F6E65546F6B656E36",
          startDate = 1000
        }
    )

marketActionToData :: PlutusTx.Data
marketActionToData = PlutusTx.toData Buy

-- Off Chain

type DropspotMarketSchema =
  Endpoint "list" ListParams
    .\/ Endpoint "buy" BuyParams

-- .\/ Endpoint "buy" ListParams
-- .\/ Endpoint "cancel" GuessParams

-- | Parameters for the "lock" endpoint
data ListParams = ListParams
  { listPolicy :: !CurrencySymbol,
    listAssetName :: !TokenName,
    listAmount :: !Integer,
    listStartDate :: !POSIXTime
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data BuyParams = BuyParams
  { buyPolicy :: !CurrencySymbol,
    buyAssetName :: !TokenName,
    buyAmount :: !Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

list :: AsContractError e => ListParams -> Contract w s e ()
list ListParams {..} = do
  pkh <- ownPaymentPubKeyHash
  let d =
        MarketDatum
          { tradeOwner = pkh,
            amount = listAmount,
            startDate = listStartDate,
            policy = listPolicy,
            token = listAssetName,
            royaltyOwner = PaymentPubKeyHash "89400023E22ECEA2CA12283BC4C65B78723905BE3306D1716E0391FD",
            royaltyPCT = 80000
          }
      v = Value.singleton listPolicy listAssetName 1 Haskell.<> Ada.lovelaceValueOf minLovelace
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @Haskell.String $ printf "Listed %s for token %s" (Haskell.show d) (Haskell.show v)

buy :: forall w s. BuyParams -> Contract w s Text ()
buy BuyParams {..} = do
  (oref, o, d@MarketDatum {..}) <- findOffer buyPolicy buyAssetName
  logInfo @Haskell.String $ printf "found offer utxo with datum %s" (Haskell.show d)

  when (buyAmount < amount) $
    throwError $ pack $ printf "offer is lower than requested amount %d" amount

  pkh <- ownPaymentPubKeyHash
  let v = Value.singleton buyPolicy buyAssetName 1 Haskell.<> Ada.lovelaceValueOf (minLovelace + buyAmount)
      t = Value.singleton buyPolicy buyAssetName 1
      r = Redeemer $ PlutusTx.toBuiltinData Buy

      lookups =
        Constraints.typedValidatorLookups typedValidator
          Haskell.<> Constraints.otherScript validator
          Haskell.<> Constraints.unspentOutputs (Map.singleton oref o)

      marketPlaceAmount = lovelacePercentage buyAmount (marketPlacePCT contractInfo)
      royaltyAmount = lovelacePercentage buyAmount royaltyPCT

      tx =
        Constraints.mustPayToPubKey (PaymentPubKeyHash $ marketPlaceOwner contractInfo) (Ada.lovelaceValueOf marketPlaceAmount)
          Haskell.<> Constraints.mustPayToPubKey royaltyOwner (Ada.lovelaceValueOf royaltyAmount)
          Haskell.<> Constraints.mustPayToPubKey tradeOwner (Ada.lovelaceValueOf (buyAmount - royaltyAmount - marketPlaceAmount - minLovelace))
          Haskell.<> Constraints.mustPayToPubKey pkh (t Haskell.<> Ada.lovelaceValueOf minLovelace)
          Haskell.<> Constraints.mustSpendScriptOutput oref r

  logInfo @Haskell.String $ printf "Attempt to Buy Token (DS Amount: %d RoyaltyAmount: %d)" marketPlaceAmount royaltyAmount
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @Haskell.String $ printf "Purchased Token (DS Amount: %d RoyaltyAmount: %d)" marketPlaceAmount royaltyAmount

findOffer :: CurrencySymbol -> TokenName -> Contract w s Text (TxOutRef, ChainIndexTxOut, MarketDatum)
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
        Just d@MarketDatum {..}
          | policy == cs && token == tn -> return (oref, o, d)
          | otherwise -> throwError "Cannot find sale"
    _ -> throwError "Cannot find Offer UTxO"

mkSchemaDefinitions ''DropspotMarketSchema

endpoints :: Contract () DropspotMarketSchema Text ()
endpoints = awaitPromise (list' `select` buy') >> endpoints
  where
    list' = endpoint @"list" list
    buy' = endpoint @"buy" buy

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

$(mkKnownCurrencies ['myToken])
