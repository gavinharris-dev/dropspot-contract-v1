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

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings   #-}


module Plutus.Contracts.DropspotMarket (dropspotMarketSBS, dropspotMarketlised, marketDatumToData, marketActionToData, valHash, scrAddress) where

import Playground.Contract

import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada


import           Codec.Serialise
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS


import Ledger.Value as Value

data ContractInfo = ContractInfo {
  marketPlaceOwner :: !PubKeyHash, -- Wallet address for Dropspot
  marketPlacePCT :: !Integer       -- Percentage to be paid to the market place owner
}

contractInfo :: ContractInfo
contractInfo = ContractInfo {
  marketPlaceOwner = "9e09ad3c6d542b0c53b36793de1c26dfa15f1c4005fd000726792c36", -- TEST Wallet
  marketPlacePCT = 400 -- 2.5%
}

data MarketDatum = MarketDatum {
  royaltyOwner :: !PubKeyHash,     -- Wallet address for Dropspot
  royaltyPCT :: !Integer,          -- Percentage to be paid to the market place owner
  tradeOwner :: !PubKeyHash,       -- Owners Wallet Public Key hash
  amount :: !Integer,              -- Minimum Trade Amount for Token
  policy :: !BuiltinByteString,    -- Policy of NFT that we are selling
  token :: !BuiltinByteString      -- NFT that we are selling
}
    deriving (Generic, ToJSON, FromJSON)

data MarketAction = Buy | Cancel
    deriving (Generic, ToJSON, FromJSON)


PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo , 0)]
PlutusTx.makeLift ''ContractInfo

PlutusTx.makeIsDataIndexed ''MarketDatum [ ('MarketDatum, 0)]
PlutusTx.makeLift ''MarketDatum

PlutusTx.makeIsDataIndexed ''MarketAction [ ('Buy, 0), ('Cancel, 1)]
PlutusTx.makeLift ''MarketAction

-- On Chain Validator

{-# INLINABLE mkValidator #-}
mkValidator :: ContractInfo -> MarketDatum -> MarketAction -> ScriptContext -> Bool
mkValidator contractInfo mkDatum mkAction context = case mkAction of
  -- Buy action need to verify that:
  --   1. The Trade Owner is being paid at least the 'minimum' amount
  --   2. The Token is being xferd to the buyer
  Buy -> 
    correctlySplit (marketPlaceOwner contractInfo) (marketPlacePCT contractInfo) (royaltyOwner mkDatum) (royaltyPCT mkDatum) (tradeOwner mkDatum) (amount mkDatum) &&
    buyerGetsPaid (valuePaidTo txInfo signer) (CurrencySymbol (policy mkDatum)) (TokenName (token mkDatum))
  -- Cancel action need to verify that:
  --   1. The Trade Owner should geti their NFT Back
  --   2. If / When we go to a Bid process the current Highest Big should be repaid.
  Cancel -> containsNFT (valuePaidTo txInfo (tradeOwner mkDatum)) (CurrencySymbol (policy mkDatum)) (TokenName (token mkDatum))

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

    correctlySplit :: PubKeyHash -> Integer -> PubKeyHash -> Integer -> PubKeyHash -> Integer -> Bool
    correctlySplit dsWallet dsPct royaltyWallet royatyPct tradeWallet total = 
      let dsAmount = lovelacePercentage total dsPct
          royaltyAmount = lovelacePercentage total royatyPct
      in
        (traceIfFalse "Dropspot Marketplace should get its share" $ Ada.fromValue (valuePaidTo txInfo dsWallet) >= Ada.lovelaceOf dsAmount) &&
        (traceIfFalse "The original Artist should get its share" $ Ada.fromValue (valuePaidTo txInfo royaltyWallet) >= Ada.lovelaceOf royaltyAmount) &&
        (traceIfFalse "The seller should get the remaining Ada" $ Ada.fromValue (valuePaidTo txInfo tradeWallet) >= Ada.lovelaceOf (total - dsAmount))

    lovelacePercentage :: Integer -> Integer -> Integer
    lovelacePercentage am p = (am * 10) `PlutusTx.Prelude.divide` p


-- Compile Section

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = MarketDatum
    type instance RedeemerType Typed = MarketAction

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractInfo)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MarketDatum @MarketAction

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

dropspotMarketSBS :: SBS.ShortByteString
dropspotMarketSBS =  SBS.toShort . LBS.toStrict $ serialise validator

dropspotMarketlised :: PlutusScript PlutusScriptV1
dropspotMarketlised = PlutusScriptSerialised dropspotMarketSBS

marketDatumToData :: PlutusTx.Data
marketDatumToData = PlutusTx.toData (MarketDatum {
  royaltyOwner = "89400023E22ECEA2CA12283BC4C65B78723905BE3306D1716E0391FD",
  royaltyPCT = 5000,
  tradeOwner = "89400023E22ECEA2CA12283BC4C65B78723905BE3306D1716E0391FD",    -- Owners Wallet Public Key hash
  amount     = 100000000,
  policy     = "103F42252C7EFC783E99035F39714F749A11C5E7408EB459630DABA9",
  token      = "416E6F746865724F6E65546F6B656E36"
})

marketActionToData :: PlutusTx.Data
marketActionToData = PlutusTx.toData Buy


-- Off Chain

-- type DropspotMarketSchema =
--         Endpoint "list" ListParams
--         .\/ Endpoint "buy" ListParams
--         -- .\/ Endpoint "cancel" GuessParams


-- -- | Parameters for the "lock" endpoint
-- data ListParams = ListParams
--     { listPolicy    :: BuiltinByteString
--     , listAssetName :: BuiltinByteString
--     , listAmount    :: Integer
--     }
--     deriving stock (Haskell.Eq, Haskell.Show, Generic)
--     deriving anyclass (FromJSON, ToJSON, ToSchema)


-- -- | The "list" contract endpoint. See note [Contract endpoints]
-- list :: AsContractError e => Promise () DropspotMarketSchema e ()
-- list = endpoint @"list" @ListParams $ \(ListParams listPolicy listAssetName listAmount) -> do
--     pkh <- Playground.Contract.ownPubKeyHash
--     let listDatum = MarketDatum { tradeOwner = pkh, amount = listAmount, policy = listPolicy, token = listAssetName }
--         tx = mustPayToTheScript listDatum (Value.singleton (CurrencySymbol listPolicy) (TokenName listAssetName) 1)
--     void $ submitTxConstraints typedValidator tx

-- buy :: AsContractError e => Promise () DropspotMarketSchema e ()
-- buy = endpoint @"buy" @ListParams $ \(ListParams listPolicy listAssetName listAmount) -> do
--     pkh <- Playground.Contract.ownPubKeyHash
--     utxos <- fundsAtAddressGeq scrAddress (getToken listPolicy listAssetName)
--     let offerUtxo = findOfferUtxo utxos (CurrencySymbol listPolicy) (TokenName listAssetName)

--     case offerUtxo of
--         [(oref, o, details)] -> do
--             let minAmount = amount details
--             if listAmount > minAmount then do
--                 let utxoMap = Map.fromList [(oref,o)]
--                 let tx = collectFromScript utxoMap Buy <>
--                         mustPayToPubKey (tradeOwner details) (Ada.lovelaceValueOf listAmount) <>
--                         mustPayToPubKey pkh (Value.singleton (CurrencySymbol listPolicy) (TokenName listAssetName) 1)
--                 void $ submitTxConstraintsSpending typedValidator utxos tx
--             else do
--                 traceError "Not offering enough ADA to buy token"
--         _ -> traceError "expected only one output"

--         where
--             getToken :: BuiltinByteString -> BuiltinByteString -> Value
--             getToken policy token = Value.singleton (CurrencySymbol policy) (TokenName token) 1

-- isOfferUtxo :: TxOutRef -> ChainIndexTxOut -> CurrencySymbol -> TokenName -> Bool
-- isOfferUtxo txOut chainIndex policy token =  txOutDatum chainIndex 

-- findOfferUtxo :: Map TxOutRef ChainIndexTxOut -> CurrencySymbol -> TokenName -> Maybe [(TxOutRef, ChainIndexTxOut)]
-- findOfferUtxo = Map.toList Map.filterWithKey isOfferUtxo

-- offerDatum :: Map TxOutRef ChainIndexTxOut -> CurrencySymbol -> TokenName -> Maybe MarketDatum
-- offerDatum = listToMaybe . catMaybes . Map.elems . Map.map getMarketDatum

-- containsNFT :: Value -> CurrencySymbol -> TokenName -> Bool
-- containsNFT v policy asset = valueOf v policy asset >= 1

-- getMarketDatum :: ChainIndexTxOut -> MarketDatum
-- getMarketDatum utxo = case txOutDatum (txOutTxOut utxo) of
--     Just h -> do
--         let [(_,datum)] = P.filter (\(h',_) -> h == h') (Map.toList (txData (txOutTxTx o)))
--         let parsedDatum = PlutusTx.fromBuiltinData (LedgerScripts.getDatum datum) :: Maybe MarketDatum
--         case parsedDatum of
--             Just b -> b
--             _ -> traceError "expected datum"
--     _ -> traceError "expectedDatum"

-- market :: AsContractError e => Contract () DropspotMarketSchema e ()
-- market = selectList [list] >> market

-- endpoints :: AsContractError e => Contract () DropspotMarketSchema e ()
-- endpoints = market

-- mkSchemaDefinitions ''DropspotMarketSchema
-- token1 = KnownCurrency (ValidatorHash "d") "Token" (TokenName "DSToken1" :| [])
-- token2 = KnownCurrency (ValidatorHash "d") "Token" (TokenName "DSToken2" :| [])
-- token3 = KnownCurrency (ValidatorHash "d") "Token" (TokenName "DSToken3" :| [])


-- $(mkKnownCurrencies ['token1, 'token2, 'token2])

