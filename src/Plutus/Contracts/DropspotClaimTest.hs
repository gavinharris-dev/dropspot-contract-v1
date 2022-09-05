{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


module Plutus.Contracts.DropspotClaimTest where

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
import Plutus.Contracts.DropspotClaim
import PlutusTx.Prelude (emptyByteString)
import Plutus.Contract.Trace (defaultDist, InitialDistribution)
import Wallet.Emulator.MultiAgent (assertOwnFundsEq)
import Plutus.Trace (callEndpoint)

ownerWallet :: Wallet
ownerWallet = knownWallet 1

buyerWallet :: Wallet
buyerWallet = knownWallet 2

listTrace :: EmulatorTrace ()
listTrace = do
  let params = Params {
    policy   = testCurr,
    token    = testToken,
    password = "TestPassword"
  }
  h1 <- activateContractWallet ownerWallet endpoints
  h2 <- activateContractWallet buyerWallet endpoints
  callEndpoint @"list" h1 params
  void $ waitUntilSlot 2
  callEndpoint @"redeem" h2 params
  void $ waitNSlots 1


listTraceFail :: EmulatorTrace ()
listTraceFail = do
  let 
    params = Params {
      policy   = testCurr,
      token    = testToken,
      password = "TestPassword"
    }

    paramsBad = Params {
      policy   = testCurr,
      token    = testToken,
      password = "WrongPassword"
    }
  h1 <- activateContractWallet ownerWallet endpoints
  h2 <- activateContractWallet buyerWallet endpoints
  callEndpoint @"list" h1 params
  void $ waitUntilSlot 2
  callEndpoint @"redeem" h2 paramsBad
  void $ waitNSlots 1

test :: IO ()
test = 
  runEmulatorTraceIO' def emCfg listTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig { 
      _initialChainState = Left $ Map.fromList ([(knownWallet i, v) | i <- [2 .. 10]] <> [(knownWallet 1, nftValue)] ),
      _slotConfig = _slotConfig (def :: EmulatorConfig),
      _feeConfig = _feeConfig (def :: EmulatorConfig)   
    }

    nftValue :: Value
    nftValue = v <> Value.singleton testCurr testToken 1

    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000


testFail :: IO ()
testFail = 
  runEmulatorTraceIO' def emCfg listTraceFail
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig { 
      _initialChainState = Left $ Map.fromList ([(knownWallet i, v) | i <- [2 .. 10]] <> [(knownWallet 1, nftValue)] ),
      _slotConfig = _slotConfig (def :: EmulatorConfig),
      _feeConfig = _feeConfig (def :: EmulatorConfig)   
    }

    nftValue :: Value
    nftValue = v <> Value.singleton testCurr testToken 1

    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000

testCurr :: CurrencySymbol
testCurr = CurrencySymbol "f"

testToken :: TokenName 
testToken = TokenName "T"