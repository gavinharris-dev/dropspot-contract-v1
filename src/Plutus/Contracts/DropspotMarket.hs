{-
  Dropspot Marketplace Plutus Smart Contract - v0.4


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
{-# LANGUAGE NumericUnderscores #-}

module Plutus.Contracts.DropspotMarket where

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
import qualified PlutusTx.Prelude as Plutus

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

data DisbursementItem = DisbursementItem {
  wallet    :: !PaymentPubKeyHash, -- Wallet address for Dropspot
  percent   :: !Integer -- Percentage to be paid to the market place owner
}
  deriving (Generic, ToJSON, FromJSON, Haskell.Show, Haskell.Eq, ToSchema)

instance Eq DisbursementItem where
  {-# INLINABLE (==) #-}
  x == y = (wallet x == wallet y) && (percent x == percent y)

data MarketDatum = MarketDatum
  { royalties :: ![DisbursementItem],
    disburements :: ![DisbursementItem],
    tradeOwner :: !PaymentPubKeyHash,
    amount :: !Integer, -- Minimum Trade Amount for Token
    policy :: !CurrencySymbol, -- Policy of NFT that we are selling
    token :: !TokenName, -- NFT that we are selling
    startDate :: !POSIXTime -- The NFT cannot be purchased before this Date
  }
  deriving (Generic, ToJSON, FromJSON, Haskell.Show)

data MarketAction = Buy | Cancel | Relist
  deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 0)]
PlutusTx.makeLift ''ContractInfo

PlutusTx.makeIsDataIndexed ''DisbursementItem [('DisbursementItem, 0)]
PlutusTx.makeLift ''DisbursementItem

PlutusTx.makeIsDataIndexed ''MarketDatum [('MarketDatum, 0)]
PlutusTx.makeLift ''MarketDatum

PlutusTx.makeIsDataIndexed ''MarketAction [('Buy, 0), ('Cancel, 1), ('Relist, 2)]
PlutusTx.makeLift ''MarketAction

-- On Chain Validator

{-# INLINEABLE minLovelace #-}
minLovelace :: Integer
minLovelace = 2_000_000 -- FIXME: We should be calculating this I think and not requiring 2 ADA on each NFT!

{-# INLINEABLE mintingFee #-}
mintingFee :: Integer
mintingFee = 5_000_000

{-# INLINEABLE lovelacePercentage #-}
lovelacePercentage :: Integer -> Integer -> Integer
lovelacePercentage am p =
  if p > 0
    then if result < minLovelace then minLovelace else result
    else 0 -- Prevent Divide By Zero
  where
    result = (am * 10) `Plutus.divide` p

{-# INLINEABLE mkValidator #-}
mkValidator :: ContractInfo -> MarketDatum -> MarketAction -> ScriptContext -> Bool
mkValidator ci mkDatum mkAction context = case mkAction of
  -- Buy action need to verify that:
  --   1. The Trade Owner is being paid at least the 'minimum' amount
  --   2. The Token is being xferd to the buyer
  Buy ->
    traceIfFalse "Sale is not open yet" (purchaseStartDatePassed $ startDate mkDatum) &&
      correctlySplit (royalties mkDatum) (marketPlaceOwner ci) (marketPlacePCT ci) (disburements mkDatum) (unPaymentPubKeyHash $ tradeOwner mkDatum) (amount mkDatum) &&
      buyerGetsPaid (valuePaidTo txInfo signer) (policy mkDatum) (token mkDatum)
      
  Relist -> 
    traceIfFalse "To relist a sale you must be the tradeOwner" $ txSignedBy txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum)
      && traceIfFalse "The Token must be paid back to the Script" tokenPaidBackToScript 
      && traceIfFalse "The output datum changes are not allowed" outputDatumChangesAreAllowed 

  Cancel ->
    traceIfFalse "The trade owner should get their NFT" $ containsNFT (valuePaidTo txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum)) (policy mkDatum) (token mkDatum) &&
    traceIfFalse "The Marketplace Owner should get their Cancelation Fee" dropspotGetPaid

  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo context

    -- We are assuming that the Wallet Signing the Transaction will be the recipient of the NFT
    signer :: PubKeyHash
    signer = case txInfoSignatories txInfo of
      [pkh] -> pkh

    tokenValue :: Value
    tokenValue = Value.singleton (policy mkDatum) (token mkDatum) 1

    ownOutput   :: TxOut
    outputDatum :: MarketDatum
    (ownOutput, outputDatum) = case getContinuingOutputs context of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h txInfo of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"

    tokenPaidBackToScript :: Bool
    tokenPaidBackToScript = txOutValue ownOutput == tokenValue Plutus.<> Ada.lovelaceValueOf minLovelace

    outputDatumChangesAreAllowed :: Bool
    outputDatumChangesAreAllowed = 
      (royalties outputDatum) == (royalties mkDatum)
        && (disburements outputDatum) == (disburements mkDatum)
        && (tradeOwner outputDatum) == (tradeOwner mkDatum)
        && (policy outputDatum) == (policy mkDatum)
        && (token outputDatum) == (token mkDatum)

    buyerGetsPaid :: Value -> CurrencySymbol -> TokenName -> Bool
    buyerGetsPaid v policy asset = traceIfFalse "The Buyer is not going to get their NFT" $ containsNFT v policy asset

    containsNFT :: Value -> CurrencySymbol -> TokenName -> Bool
    containsNFT v policy asset = valueOf v policy asset >= 1

    dropspotGetPaid :: Bool
    dropspotGetPaid = Ada.fromValue (valuePaidTo txInfo (marketPlaceOwner ci)) >= Ada.lovelaceOf mintingFee

    correctlySplit :: [DisbursementItem] -> PubKeyHash -> Integer -> [DisbursementItem] -> PubKeyHash -> Integer -> Bool
    correctlySplit royalties dsWallet dsPct dis to total =
      let dsAmount = lovelacePercentage total dsPct
          royaltyAmount = PlutusTx.Prelude.sum $ PlutusTx.Prelude.map (lovelacePercentage total . percent) royalties
          remainingAmount = total - dsAmount - royaltyAmount - minLovelace

          tradeOwnerPayout = remainingAmount - otherDisbursements remainingAmount dis

       in traceIfFalse "Dropspot Marketplace should get its share" (Ada.fromValue (valuePaidTo txInfo dsWallet) >= Ada.lovelaceOf dsAmount)
            && traceIfFalse "The Royalties should be paid out" (royaltiesPaidOut total royalties)
            && traceIfFalse "The remaining ADA should be correctly Disbursed" (currectlyDisbursed remainingAmount dis)
            && traceIfFalse "The tradeOwner should get all that is left" (Ada.fromValue (valuePaidTo txInfo to) >= Ada.lovelaceOf tradeOwnerPayout)

    otherDisbursements :: Integer -> [DisbursementItem] -> Integer
    otherDisbursements t d = Foldable.sum $ PlutusTx.Prelude.map (\d1 -> lovelacePercentage t $ percent d1) d

    currectlyDisbursed :: Integer -> [DisbursementItem] -> Bool
    currectlyDisbursed amt ds =  Foldable.all (\d -> Ada.fromValue (valuePaidTo txInfo $ unPaymentPubKeyHash (wallet d)) >= (Ada.lovelaceOf $ lovelacePercentage amt $ percent d) ) ds

    royaltiesPaidOut :: Integer -> [DisbursementItem] -> Bool
    royaltiesPaidOut total ra =
      Foldable.all (\r -> Ada.fromValue (valuePaidTo txInfo $ unPaymentPubKeyHash (wallet r)) >= (Ada.lovelaceOf $ lovelacePercentage total $ percent r)) ra

    purchaseStartDatePassed :: POSIXTime -> Bool
    purchaseStartDatePassed startDate = contains (from startDate) (txInfoValidRange txInfo)

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
    listStartDate :: !POSIXTime,
    listTradeOwner :: !PaymentPubKeyHash,
    listDisbursements :: ![DisbursementItem],
    listRoyalties :: ![DisbursementItem]
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

data RelistParams = RelistParams {
  relistAmount :: Integer,
  relistStartDate :: POSIXTime,
  relistPolicy :: !CurrencySymbol,
  relistAssetName :: !TokenName
}
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)


list :: AsContractError e => ListParams -> Contract w s e ()
list ListParams {..} = do
  pkh <- ownPaymentPubKeyHash
  let d =
        MarketDatum
          {
            disburements = listDisbursements,
            amount = listAmount,
            tradeOwner = listTradeOwner,
            startDate = listStartDate,
            policy = listPolicy,
            token = listAssetName,
            royalties = listRoyalties
          }
      v = Value.singleton listPolicy listAssetName 1 Haskell.<> Ada.lovelaceValueOf minLovelace
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @Haskell.String $ printf "Listed %s for token %s. Transaction %s" (Haskell.show d) (Haskell.show v) (Haskell.show ledgerTx)

-- relist :: forall w s. RelistParams -> Contract w s Text ()
-- relist RelistParams {..} = do
--   (oref, o, d@MarketDatum {..}) <- findOffer relistPolicy relistAssetName
  
--   pkh <- ownPaymentPubKeyHash
--   let r  = Redeemer $ PlutusTx.toBuiltinData Relist
--       lookups =
--         Constraints.typedValidatorLookups typedValidator
--           Haskell.<> Constraints.ownPaymentPubKeyHash pkh
--           Haskell.<> Constraints.otherScript validator
--           Haskell.<> Constraints.unspentOutputs (Map.singleton oref o)

--       tx = Constraints.mustSpendScriptOutput oref r


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
          Haskell.<> Constraints.ownPaymentPubKeyHash pkh
          Haskell.<> Constraints.otherScript validator
          Haskell.<> Constraints.unspentOutputs (Map.singleton oref o)

      marketPlaceAmount = lovelacePercentage buyAmount (marketPlacePCT contractInfo)
      totalRoyaltyAmount = Haskell.sum $ Haskell.map (lovelacePercentage buyAmount . percent) royalties

      disbursementAmount = buyAmount - totalRoyaltyAmount - marketPlaceAmount - minLovelace

      otherDisbursements = Haskell.sum $ Haskell.map (\d -> lovelacePercentage disbursementAmount (percent d)) disburements

      remainingAmount = disbursementAmount - otherDisbursements
      tx =
        Constraints.mustPayToPubKey (PaymentPubKeyHash $ marketPlaceOwner contractInfo) (Ada.lovelaceValueOf marketPlaceAmount)
          Haskell.<>  Haskell.foldl (Haskell.<>) mempty (
            Haskell.map (
              \r -> Constraints.mustPayToPubKey (wallet r) (Ada.lovelaceValueOf (lovelacePercentage disbursementAmount (percent r)))) disburements
          )
          Haskell.<> Constraints.mustPayToPubKey tradeOwner (Ada.lovelaceValueOf remainingAmount)
          Haskell.<> Constraints.mustSpendAtLeast (Ada.lovelaceValueOf buyAmount)
          Haskell.<> Constraints.mustPayToPubKey pkh (t Haskell.<> Ada.lovelaceValueOf minLovelace)
          Haskell.<> Constraints.mustValidateIn (from startDate)
          Haskell.<> Constraints.mustSpendScriptOutput oref r
          Haskell.<> Haskell.foldl (Haskell.<>) mempty (Haskell.map (
            \r -> Constraints.mustPayToPubKey (wallet r) (Ada.lovelaceValueOf (lovelacePercentage buyAmount (percent r)))) royalties)

  logInfo @Haskell.String $ printf "Attempt to Buy Token for %d (DS Amount: %d RoyaltyAmount: %d, disbursementAmount %d (remainingAmount: %d))" buyAmount marketPlaceAmount totalRoyaltyAmount disbursementAmount remainingAmount
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @Haskell.String $ printf "Purchased Token (DS Amount: %d RoyaltyAmount: %d)" marketPlaceAmount totalRoyaltyAmount

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
