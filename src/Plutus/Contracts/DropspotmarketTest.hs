{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


module Plutus.Contracts.DropspotmarketTest where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.V1.Ledger.Value
import PlutusTx.Builtins
import Ledger.Ada as Ada;
import Ledger.Value as Value
import Ledger.Address

import Data.Map as Map
import Plutus.Contracts.DropspotMarket
import PlutusTx.Prelude (emptyByteString)
import Plutus.Contract.Trace (defaultDist, InitialDistribution)
import Wallet.Emulator.MultiAgent (assertOwnFundsEq)
import Plutus.Trace (callEndpoint)


ownerWallet :: Wallet
ownerWallet = knownWallet 1

buyerWallet :: Wallet
buyerWallet = knownWallet 2

dsWallet :: Wallet
dsWallet = knownWallet 3

charityWallet :: Wallet
charityWallet = knownWallet 4

royaltyWallet :: Wallet
royaltyWallet = knownWallet 5
royaltyWallet2 :: Wallet
royaltyWallet2 = knownWallet 6

testCurr :: CurrencySymbol
testCurr = CurrencySymbol "f"

testToken :: TokenName 
testToken = TokenName "T"



listTrace :: EmulatorTrace ()
listTrace = do
  h1 <- activateContractWallet ownerWallet endpoints
  h2 <- activateContractWallet buyerWallet endpoints
  callEndpoint @"list" h1 $ ListParams {
    listPolicy = testCurr,
    listAssetName = testToken,
    listAmount = 400_000_000,
    listStartDate = slotToBeginPOSIXTime def 20,
    listTradeOwner = mockWalletPaymentPubKeyHash ownerWallet,
    listDisbursements = [
      DisbursementItem (mockWalletPaymentPubKeyHash charityWallet) 500 -- 100%
    ],
    listRoyalties = [
      DisbursementItem (mockWalletPaymentPubKeyHash royaltyWallet) 1000, -- 1%
      DisbursementItem (mockWalletPaymentPubKeyHash royaltyWallet2) 666 -- 1%
    ]
  }
  void $ waitUntilSlot 23
  callEndpoint @"buy" h2 $ BuyParams {
    buyPolicy = testCurr,
    buyAssetName  = testToken,
    buyAmount = 400_000_000
  }
  void $ waitNSlots 1


listTraceNoRoyalties :: EmulatorTrace ()
listTraceNoRoyalties = do
  h1 <- activateContractWallet ownerWallet endpoints
  h2 <- activateContractWallet buyerWallet endpoints
  callEndpoint @"list" h1 $ ListParams {
    listPolicy = testCurr,
    listAssetName = testToken,
    listAmount = 400_000_000,
    listTradeOwner = mockWalletPaymentPubKeyHash ownerWallet,
    listStartDate = slotToBeginPOSIXTime def 20,
    listDisbursements = [
      DisbursementItem (mockWalletPaymentPubKeyHash charityWallet) 500 -- 100%
    ],
    listRoyalties = []
  }
  void $ waitUntilSlot 23
  callEndpoint @"buy" h2 $ BuyParams {
    buyPolicy = testCurr,
    buyAssetName  = testToken,
    buyAmount = 400_000_000
  }
  void $ waitNSlots 1


listTraceNoRoyaltiesOrDisbursements :: EmulatorTrace ()
listTraceNoRoyaltiesOrDisbursements = do
  h1 <- activateContractWallet ownerWallet endpoints
  h2 <- activateContractWallet buyerWallet endpoints
  callEndpoint @"list" h1 $ ListParams {
    listPolicy = testCurr,
    listAssetName = testToken,
    listAmount = 400_000_000,
    listTradeOwner = mockWalletPaymentPubKeyHash ownerWallet,
    listStartDate = slotToBeginPOSIXTime def 20,
    listDisbursements = [],
    listRoyalties = []
  }
  void $ waitUntilSlot 23
  callEndpoint @"buy" h2 $ BuyParams {
    buyPolicy = testCurr,
    buyAssetName  = testToken,
    buyAmount = 400_000_000
  }
  void $ waitNSlots 1


listTraceReList :: EmulatorTrace ()
listTraceReList = do
  h1 <- activateContractWallet ownerWallet endpoints
  callEndpoint @"list" h1 $ ListParams {
    listPolicy = testCurr,
    listAssetName = testToken,
    listAmount = 400_000_000,
    listTradeOwner = mockWalletPaymentPubKeyHash ownerWallet,
    listStartDate = slotToBeginPOSIXTime def 20,
    listDisbursements = [],
    listRoyalties = []
  }
  void $ waitNSlots 2


test :: IO ()
test = runEmulatorTraceIO' def emCfg listTraceNoRoyaltiesOrDisbursements
  where
    
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig { 
      _initialChainState = Left $ Map.fromList ([(knownWallet i, v) | i <- [2 .. 10]] <> [(knownWallet 1, nftValue)] ),
      _slotConfig = _slotConfig (def :: EmulatorConfig),
      _feeConfig = _feeConfig (def :: EmulatorConfig)   
    }
  
    nftValue :: Value
    nftValue = v <> Value.singleton testCurr testToken 1 
                 <> Value.singleton (CurrencySymbol "1413b3f27eaa75905f3c88b82fd4984904023f3606d8b576232a87f9") (TokenName "tDMT.1") 1
                 <> Value.singleton (CurrencySymbol "1413b3f27eaa75905f3c88b82fd4984904023f3606d8b576232a87f9") (TokenName "tDMT.2") 1
                 <> Value.singleton (CurrencySymbol "1413b3f27eaa75905f3c88b82fd4984904023f3606d8b576232a87f9") (TokenName "tDMT.3") 1

    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000


-- [
--     102_0([
--         0,
--         [
--             [],
--             [],
--             h'd006eb7783e8c93160b2bab287bc8a6f069e9e690cd82bc0b52a8c31',
--             70000002_2,
--             h'777be88df242bd81b95d8947d099086abf1896a4d693be4a80388ee9',
--             h'74444d54',
--             1652935982942_3,
--         ],
--     ]),
-- ],

testDatum :: MarketDatum
testDatum = MarketDatum {
  royalties = [],
  disburements = [],
  tradeOwner = PaymentPubKeyHash "d006eb7783e8c93160b2bab287bc8a6f069e9e690cd82bc0b52a8c31",
  amount = 70000002,
  policy = CurrencySymbol "777be88df242bd81b95d8947d099086abf1896a4d693be4a80388ee9",
  token = TokenName "74444d54",
  startDate = 1652935982942
}