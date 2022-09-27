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
{-# LANGUAGE CPP #-}

module Plutus.Contracts.DropspotMarket where

#define MIN_UTXO_AMOUNT 2000000
#define DROPSPOT_MINTING_FEE 5000000

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
import Plutus.V1.Ledger.Value as Value
import Plutus.V1.Ledger.Contexts as Contexts
import Playground.Contract
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import qualified Plutus.Contract.Constraints as Constraints
import Plutus.Contract.Request (awaitTxConfirmed)
import Plutus.Contract.Types (AsContractError)
import qualified Plutus.V1.Ledger.Ada as Ada
import Plutus.V1.Ledger.Interval (from, before)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified PlutusTx.Foldable as Foldable
import qualified PlutusTx.AssocMap as PlutusMap
import Text.Printf (printf)
import qualified Prelude as Haskell
import qualified PlutusTx.Prelude as Plutus
import Cardano.Binary
import qualified Plutus.V1.Ledger.Scripts as LS
import qualified Ledger.Typed.Scripts.Validators as SV

data ContractInfo = ContractInfo
  { marketPlaceOwner :: !PaymentPubKeyHash, -- Wallet address for Dropspot
    marketPlacePCT :: !Integer -- Percentage to be paid to the market place owner
  }

-- // TESTNET COMPILE
-- contractInfo :: ContractInfo
-- contractInfo =
--   ContractInfo
--     { marketPlaceOwner = PaymentPubKeyHash "9e09ad3c6d542b0c53b36793de1c26dfa15f1c4005fd000726792c36", -- TEST Wallet
--       marketPlacePCT = 400 -- 2.5%
--     }

-- -- // MAINNET COMPILE
-- contractInfo :: ContractInfo
-- contractInfo =
--   ContractInfo
--     { marketPlaceOwner = PaymentPubKeyHash "8714132c8367303702bfeacbbe06be8f4b9442e00f1ddd0529347947", -- TEST Wallet
--       marketPlacePCT = 400 -- 2.5%
--     }

-- // PREPROD COMPILE
-- contractInfo :: ContractInfo
-- contractInfo =
--   ContractInfo
--     { marketPlaceOwner = PaymentPubKeyHash "24d7811ebd7aff17e6be4b2f7a3677886f9617973e119855da033bb9", -- TEST Wallet
--       marketPlacePCT = 400 -- 2.5%
--     }


data DisbursementItem = DisbursementItem {
  wallet    :: !PaymentPubKeyHash, -- Wallet address for Dropspot
  percent   :: !Integer -- Percentage to be paid to the market place owner
}
  deriving (Generic, ToJSON, FromJSON, Haskell.Show, Haskell.Eq, ToSchema)

-- instance ToCBOR DisbursementItem where
--   toCBOR di = 
--       toCBOR (getPubKeyHash $ unPaymentPubKeyHash (wallet di)) Haskell.<> toCBOR (percent di)



instance Eq DisbursementItem where
  {-# INLINABLE (==) #-}
  x == y = wallet x == wallet y && percent x == percent y

data MarketDatum = MarketDatum
  { 
    royalties :: [DisbursementItem],
    disburements :: [DisbursementItem],
    tradeOwner :: PaymentPubKeyHash,
    amount :: Integer, -- Minimum Trade Amount for Token
    policy :: CurrencySymbol, -- Policy of NFT that we are selling
    token :: TokenName, -- NFT that we are selling
    startDate :: POSIXTime -- The NFT cannot be purchased before this Date
  }
  deriving (Generic, ToJSON, FromJSON, Haskell.Show)

-- data MarketAction = Buy | Relist | Cancel | CCBuy
data MarketAction = Buy | Relist | Cancel

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 0)]
PlutusTx.makeLift ''ContractInfo

PlutusTx.makeIsDataIndexed ''DisbursementItem [('DisbursementItem, 0)]
PlutusTx.makeLift ''DisbursementItem

PlutusTx.makeIsDataIndexed ''MarketDatum [('MarketDatum, 0)]
PlutusTx.makeLift ''MarketDatum

-- PlutusTx.makeIsDataIndexed ''MarketAction [('Buy, 0), ('Relist, 1), ('Cancel, 2), ('CCBuy, 3)]

PlutusTx.makeIsDataIndexed ''MarketAction [('Buy, 0), ('Relist, 1), ('Cancel, 2)]
PlutusTx.makeLift ''MarketAction

-- On Chain Validator

{-# INLINEABLE mkValidator #-}
mkValidator :: ContractInfo -> MarketDatum -> MarketAction -> Contexts.ScriptContext -> Bool
mkValidator ci mkDatum mkAction context = case mkAction of
  -- Buy action need to verify that:
  --   1. The Trade Owner is being paid at least the 'minimum' amount
  --   2. The Token is being xferd to the buyer
  Buy -> standardBuyConditions   
      -- The Signing Wallet is paid out the NFT
      -- && Foldable.any (\s -> containsNFT (valuePaidTo txInfo s) (policy mkDatum) (token mkDatum)) (txInfoSignatories txInfo)
      -- && Foldable.length (txInfoSignatories txInfo) >= 1

  -- CCBuy -> 
  --         standardBuyConditions 
  --     &&  txSignedBy txInfo (unPaymentPubKeyHash $ marketPlaceOwner ci)

  Relist ->
          txSignedBy txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum)
      &&  tokenPaidBackToScript
      &&  outputDatumChangesAreAllowed

  Cancel -> -- Trade Owner gets the Token and DS gets its Minting Fee, as long as the Transaction is signed 
          txSignedBy txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum)   -- (txSignedBy txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum) || txSignedBy txInfo (unPaymentPubKeyHash $ marketPlaceOwner ci))
      &&  Value.valueOf (valuePaidTo txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum)) (policy mkDatum) (token mkDatum) >= 1 -- Should we be (in Datum) holding a ref to the number of Tokens that are being 'sold'?
      &&  (Ada.getLovelace (Ada.fromValue (valuePaidTo txInfo (unPaymentPubKeyHash $ marketPlaceOwner ci))) >= DROPSPOT_MINTING_FEE) 

  where

    standardBuyConditions :: Bool
    standardBuyConditions = (purchaseStartDatePassed $ startDate mkDatum) 
      &&  (currectlyDisbursed (amount mkDatum) (royalties mkDatum)) 
      &&  (currectlyDisbursed remainingAmount (disburements mkDatum)) 
      &&  (Ada.getLovelace (Ada.fromValue (valuePaidTo txInfo (unPaymentPubKeyHash $ marketPlaceOwner ci))) >= dsAmount) 
      &&  (Ada.getLovelace (Ada.fromValue (valuePaidTo txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum))) >= tradeOwnerPayout) 


    lovelacePercentage :: Integer -> Integer -> Integer
    lovelacePercentage am p = (am * 10) `Plutus.divide` p

    dsAmount :: Integer
    dsAmount = lovelacePercentage (amount mkDatum) (marketPlacePCT ci)

    royaltyAmount :: Integer
    royaltyAmount = PlutusTx.Prelude.sum $ PlutusTx.Prelude.map (lovelacePercentage (amount mkDatum) . percent) (royalties mkDatum)
    
    remainingAmount :: Integer
    remainingAmount = amount mkDatum - dsAmount - royaltyAmount - MIN_UTXO_AMOUNT

    tradeOwnerPayout :: Integer
    tradeOwnerPayout = remainingAmount - otherDisbursements remainingAmount (disburements mkDatum)

    txInfo :: TxInfo
    txInfo = scriptContextTxInfo context

    ownOutput   :: TxOut
    outputDatum :: MarketDatum
    (ownOutput, outputDatum) = case getContinuingOutputs context of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "E1"
            Just h -> case findDatum h txInfo of
                Nothing        -> traceError "E2"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "E3"
        _   -> traceError "E4"

    tokenPaidBackToScript :: Bool
    tokenPaidBackToScript = Value.valueOf (txOutValue ownOutput) (policy mkDatum) (token mkDatum) >= 1
      && Ada.getLovelace (Ada.fromValue (txOutValue ownOutput)) >= MIN_UTXO_AMOUNT

    outputDatumChangesAreAllowed :: Bool
    outputDatumChangesAreAllowed =
      royalties outputDatum == royalties mkDatum
        && tradeOwner outputDatum == tradeOwner mkDatum
        && policy outputDatum == policy mkDatum
        && token outputDatum == token mkDatum

    containsNFT :: Value -> CurrencySymbol -> TokenName -> Bool
    containsNFT v policy asset = Value.valueOf v policy asset >= 1

    otherDisbursements :: Integer -> [DisbursementItem] -> Integer
    otherDisbursements t d = Foldable.sum $ PlutusTx.Prelude.map (lovelacePercentage t . percent) d

    currectlyDisbursed :: Integer -> [DisbursementItem] -> Bool
    currectlyDisbursed amt ds =  Foldable.all (\d -> Ada.getLovelace (Ada.fromValue (valuePaidTo txInfo $ unPaymentPubKeyHash (wallet d))) >= lovelacePercentage amt (percent d)) ds

    purchaseStartDatePassed :: POSIXTime -> Bool
    purchaseStartDatePassed startDate = Plutus.V1.Ledger.Interval.before startDate (txInfoValidRange txInfo)

-- Compile Section

data Typed

instance Scripts.ValidatorTypes Typed where
  type DatumType Typed = MarketDatum
  type RedeemerType Typed = MarketAction

typedValidator :: ContractInfo -> Scripts.TypedValidator Typed
typedValidator ci =
  Scripts.mkTypedValidator @Typed
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode ci)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @MarketDatum @MarketAction

validator :: ContractInfo -> Validator
validator ci = Scripts.validatorScript $ typedValidator ci

valHash :: ContractInfo -> Ledger.ValidatorHash
valHash ci = Scripts.validatorHash $ typedValidator ci

scrAddress :: ContractInfo -> Ledger.Address
scrAddress ci = scriptAddress  $ validator ci

valAddressHash :: ContractInfo -> Ledger.Address
valAddressHash ci = Scripts.validatorAddress $ typedValidator ci

dropspotMarketSBS :: ContractInfo -> SBS.ShortByteString
dropspotMarketSBS ci = SBS.toShort . LBS.toStrict $ serialise $ validator ci

dropspotMarketlised :: ContractInfo -> PlutusScript PlutusScriptV1
dropspotMarketlised ci = PlutusScriptSerialised $ dropspotMarketSBS ci

dsScriptSize :: ContractInfo -> Integer
dsScriptSize ci = LS.scriptSize $ LS.getValidator $ SV.validatorScript $ typedValidator ci

{-
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
      v = Value.singleton listPolicy listAssetName 1 Haskell.<> Ada.lovelaceValueOf MIN_UTXO_AMOUNT
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints (typedValidator contractInfo) tx
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
  let v = Value.singleton buyPolicy buyAssetName 1 Haskell.<> Ada.lovelaceValueOf (MIN_UTXO_AMOUNT + buyAmount)
      t = Value.singleton buyPolicy buyAssetName 1
      r = Redeemer $ PlutusTx.toBuiltinData Buy
      lookups =
        Constraints.typedValidatorLookups (typedValidator contractInfo)
          Haskell.<> Constraints.ownPaymentPubKeyHash pkh
          Haskell.<> Constraints.otherScript (validator contractInfo)
          Haskell.<> Constraints.unspentOutputs (Map.singleton oref o)

      marketPlaceAmount = lovelacePercentage buyAmount (marketPlacePCT contractInfo)
      totalRoyaltyAmount = Haskell.sum $ Haskell.map (lovelacePercentage buyAmount . percent) royalties

      disbursementAmount = buyAmount - totalRoyaltyAmount - marketPlaceAmount - MIN_UTXO_AMOUNT

      otherDisbursements = Haskell.sum $ Haskell.map (lovelacePercentage disbursementAmount . percent) disburements

      remainingAmount = disbursementAmount - otherDisbursements
      tx =
        Constraints.mustPayToPubKey (PaymentPubKeyHash $ marketPlaceOwner contractInfo) (Ada.lovelaceValueOf marketPlaceAmount)
          Haskell.<>  Haskell.foldl (Haskell.<>) mempty (
            Haskell.map (
              \r -> Constraints.mustPayToPubKey (wallet r) (Ada.lovelaceValueOf (lovelacePercentage disbursementAmount (percent r)))) disburements
          )
          Haskell.<> Constraints.mustPayToPubKey tradeOwner (Ada.lovelaceValueOf remainingAmount)
          Haskell.<> Constraints.mustSpendAtLeast (Ada.lovelaceValueOf buyAmount)
          Haskell.<> Constraints.mustPayToPubKey pkh (t Haskell.<> Ada.lovelaceValueOf MIN_UTXO_AMOUNT)
          Haskell.<> Constraints.mustValidateIn (from startDate)
          Haskell.<> Constraints.mustSpendScriptOutput oref r
          Haskell.<> Haskell.foldl (Haskell.<>) mempty (Haskell.map (
            \r -> Constraints.mustPayToPubKey (wallet r) (Ada.lovelaceValueOf (lovelacePercentage buyAmount (percent r)))) royalties)

  logInfo @Haskell.String $ printf "Attempt to Buy Token for %d (DS Amount: %d RoyaltyAmount: %d, disbursementAmount %d (remainingAmount: %d))" buyAmount marketPlaceAmount totalRoyaltyAmount disbursementAmount remainingAmount

  myTxn <- mkTxConstraints lookups tx

  logInfo @Haskell.String $ printf ">>>> TXN: %s" (Haskell.show myTxn)

  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @Haskell.String $ printf "Purchased Token (DS Amount: %d RoyaltyAmount: %d)" marketPlaceAmount totalRoyaltyAmount

findOffer :: CurrencySymbol -> TokenName -> Contract w s Text (TxOutRef, ChainIndexTxOut, MarketDatum)
findOffer cs tn = do
  utxos <- utxosAt $ scriptHashAddress (valHash contractInfo)
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
-}