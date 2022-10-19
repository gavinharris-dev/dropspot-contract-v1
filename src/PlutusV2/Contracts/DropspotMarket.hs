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
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE CPP                        #-}

module PlutusV2.Contracts.DropspotMarket where

#define MIN_UTXO_AMOUNT 2000000
#define DROPSPOT_MINTING_FEE 5000000

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise
import Control.Monad hiding (fmap)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Map as Map
import Data.Text (Text, pack)
import qualified Ledger as Ledger
import Ledger.Ada as Ada
import Ledger.Address
import qualified Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.V1.Ledger.Value as Value
import Plutus.V2.Ledger.Contexts as Contexts

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

import Cardano.Api (writeFileTextEnvelope, displayError)

import qualified PlutusTx.Builtins as PB
import qualified Shrink.PlutusTX as ShrinkP


data ContractInfo = ContractInfo
  { marketPlaceOwner :: BuiltinByteString, -- Wallet address for Dropspot
    marketPlacePCT :: !Integer -- Percentage to be paid to the market place owner
  }
  deriving (Generic, ToJSON, FromJSON, Haskell.Show, ToSchema)

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 0)]
PlutusTx.makeLift ''ContractInfo

data DisbursementItem = DisbursementItem {
  wallet    :: !PaymentPubKeyHash, -- Wallet address for Dropspot
  percent   :: !Integer -- Percentage to be paid to the market place owner
}
  deriving (Generic, ToJSON, FromJSON, Haskell.Show, Haskell.Eq, ToSchema)

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
    startDate :: Ledger.POSIXTime -- The NFT cannot be purchased before this Date
  }
  deriving (Generic, ToJSON, FromJSON, Haskell.Show)

-- data MarketAction = Buy | Relist | Cancel | CCBuy
data MarketAction = Buy | Relist | Cancel

PlutusTx.makeIsDataIndexed ''DisbursementItem [('DisbursementItem, 0)]
PlutusTx.makeLift ''DisbursementItem

PlutusTx.makeIsDataIndexed ''MarketDatum [('MarketDatum, 0)]
PlutusTx.makeLift ''MarketDatum

-- PlutusTx.makeIsDataIndexed ''MarketAction [('Buy, 0), ('Relist, 1), ('Cancel, 2), ('CCBuy, 3)]

PlutusTx.makeIsDataIndexed ''MarketAction [('Buy, 0), ('Relist, 1), ('Cancel, 2)]
PlutusTx.makeLift ''MarketAction

-- On Chain Validator

{-# INLINEABLE mkValidator #-}
mkValidator :: MarketDatum -> MarketAction -> ScriptContext -> Bool
mkValidator mkDatum mkAction context = case mkAction of
  -- Buy action need to verify that:
  --   1. The Trade Owner is being paid at least the 'minimum' amount
  --   2. The Token is being xferd to the buyer
  Buy -> standardBuyConditions   
      -- The Signing Wallet is paid out the NFT
      -- && Foldable.any (\s -> containsNFT (Contexts.valuePaidTo txInfo s) (policy mkDatum) (token mkDatum)) (txInfoSignatories txInfo)
      -- && Foldable.length (txInfoSignatories txInfo) >= 1

  -- CCBuy -> 
  --         standardBuyConditions 
  --     &&  txSignedBy txInfo (marketPlaceOwner contractInfo)

  Relist ->
          txSignedBy txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum)
      &&  tokenPaidBackToScript
      &&  outputDatumChangesAreAllowed

  Cancel -> -- Trade Owner gets the Token and DS gets its Minting Fee, as long as the Transaction is signed 
          (txSignedBy txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum) || txSignedBy txInfo (Ledger.PubKeyHash $ marketPlaceOwner contractInfo))
      &&  Value.valueOf (Contexts.valuePaidTo txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum)) (policy mkDatum) (token mkDatum) >= 1 -- Should we be (in Datum) holding a ref to the number of Tokens that are being 'sold'?
      &&  (Ada.getLovelace (Ada.fromValue (Contexts.valuePaidTo txInfo (Ledger.PubKeyHash $ marketPlaceOwner contractInfo))) >= DROPSPOT_MINTING_FEE) 

  where

    txInfo :: TxInfo
    txInfo = scriptContextTxInfo context

    contractInfo :: ContractInfo
    contractInfo = ContractInfo {
      marketPlaceOwner = "24d7811ebd7aff17e6be4b2f7a3677886f9617973e119855da033bb9",
      marketPlacePCT   = 400
    }

    standardBuyConditions :: Bool
    standardBuyConditions = (purchaseStartDatePassed $ startDate mkDatum) 
      &&  (currectlyDisbursed (amount mkDatum) (royalties mkDatum)) 
      &&  (currectlyDisbursed remainingAmount (disburements mkDatum)) 
      &&  (Ada.getLovelace (Ada.fromValue (valuePaidTo txInfo (Ledger.PubKeyHash $ marketPlaceOwner contractInfo))) >= dsAmount) 
      &&  (Ada.getLovelace (Ada.fromValue (valuePaidTo txInfo (unPaymentPubKeyHash $ tradeOwner mkDatum))) >= tradeOwnerPayout) 


    lovelacePercentage :: Integer -> Integer -> Integer
    lovelacePercentage am p = (am * 10) `Plutus.divide` p

    dsAmount :: Integer
    dsAmount = lovelacePercentage (amount mkDatum) (marketPlacePCT contractInfo)

    royaltyAmount :: Integer
    royaltyAmount = PlutusTx.Prelude.sum $ PlutusTx.Prelude.map (lovelacePercentage (amount mkDatum) . percent) (royalties mkDatum)
    
    remainingAmount :: Integer
    remainingAmount = amount mkDatum - dsAmount - royaltyAmount - MIN_UTXO_AMOUNT

    tradeOwnerPayout :: Integer
    tradeOwnerPayout = remainingAmount - otherDisbursements remainingAmount (disburements mkDatum)

    otherDisbursements :: Integer -> [DisbursementItem] -> Integer
    otherDisbursements t d = Foldable.sum $ PlutusTx.Prelude.map (lovelacePercentage t . percent) d
    
    currectlyDisbursed :: Integer -> [DisbursementItem] -> Bool
    currectlyDisbursed amt ds =  Foldable.all (\d -> Ada.getLovelace (Ada.fromValue (valuePaidTo txInfo $ unPaymentPubKeyHash (wallet d))) >= lovelacePercentage amt (percent d)) ds


    ownOutput   :: TxOut
    outputDatum :: MarketDatum
    (ownOutput, outputDatum) = case Contexts.getContinuingOutputs context of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "E1"
            Just h -> case Contexts.findDatum h txInfo of
                Nothing        -> traceError "E2"
                Just (LS.Datum d) ->  case PlutusTx.fromBuiltinData d of
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

    -- containsNFT :: Value -> CurrencySymbol -> TokenName -> Bool
    -- containsNFT v policy asset = Value.valueOf v policy asset >= 1
   
    purchaseStartDatePassed :: Ledger.POSIXTime -> Bool
    purchaseStartDatePassed startDate = Plutus.V1.Ledger.Interval.before startDate (txInfoValidRange txInfo)

-- Compile Section

mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator datum redeemer ctx = Plutus.check (
  mkValidator (PlutusTx.unsafeFromBuiltinData datum :: MarketDatum) (PlutusTx.unsafeFromBuiltinData redeemer :: MarketAction) (PlutusTx.unsafeFromBuiltinData ctx :: ScriptContext))

dsMarketValidator :: LS.Validator
dsMarketValidator = LS.mkValidatorScript $$(PlutusTx.compile [||mkWrappedValidator||])


dsMarketScript :: LS.Script
dsMarketScript = LS.unValidatorScript dsMarketValidator

dsMarketSBS :: SBS.ShortByteString
dsMarketSBS = SBS.toShort . LBS.toStrict $ serialise dsMarketValidator

dsMarketSerialised :: PlutusScript PlutusScriptV2
dsMarketSerialised = PlutusScriptSerialised dsMarketSBS

writeDropspotMarketScriptV2 :: IO ()
writeDropspotMarketScriptV2 = do 
  result <- writeFileTextEnvelope "ds-market-v2.s.plutus" Nothing dsMarketSerialised
  case result of
    Left err -> Haskell.print $ displayError err
    Right () -> return ()


-- data Typed

-- instance Scripts.ValidatorTypes Typed where
--   type DatumType Typed = MarketDatum
--   type RedeemerType Typed = MarketAction

-- typedValidator :: ContractInfo -> Scripts.TypedValidator Typed
-- typedValidator ci =
--   Scripts.mkTypedValidator @Typed
--     ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode ci)
--     $$(PlutusTx.compile [||wrap||])
--   where
--     wrap = Scripts.wrapValidator @MarketDatum @MarketAction

-- validator :: ContractInfo -> Validator
-- validator ci = Scripts.validatorScript $ typedValidator ci

-- valHash :: ContractInfo -> Ledger.ValidatorHash
-- valHash ci = Scripts.validatorHash $ typedValidator ci

-- scrAddress :: ContractInfo -> Ledger.Address
-- scrAddress ci = scriptAddress  $ validator ci

-- valAddressHash :: ContractInfo -> Ledger.Address
-- valAddressHash ci = Scripts.validatorAddress $ typedValidator ci

-- dropspotMarketSBS :: ContractInfo -> SBS.ShortByteString
-- dropspotMarketSBS ci = SBS.toShort . LBS.toStrict $ serialise $ validator ci

-- dropspotMarketlised :: ContractInfo -> PlutusScript PlutusScriptV1
-- dropspotMarketlised ci = PlutusScriptSerialised $ dropspotMarketSBS ci

-- dsScriptSize :: ContractInfo -> Integer
-- dsScriptSize ci = LS.scriptSize $ LS.getValidator $ SV.validatorScript $ typedValidator ci

