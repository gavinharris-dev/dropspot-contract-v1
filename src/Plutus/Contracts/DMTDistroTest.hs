{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Plutus.Contracts.DMTDistroTest where

import Plutus.Contracts.DropspotDMTDistro
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
import Ledger.Address (unPaymentPubKeyHash)

import Data.Map as Map
import PlutusTx.Prelude (emptyByteString)
import Plutus.Contract.Trace (defaultDist, InitialDistribution)
import Wallet.Emulator.MultiAgent (assertOwnFundsEq)
import Plutus.Trace (callEndpoint)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)

dsWallet :: Wallet
dsWallet = knownWallet 1

userWallet :: Wallet
userWallet = knownWallet 2

testCurr :: CurrencySymbol
testCurr = CurrencySymbol "777be88df242bd81b95d8947d099086abf1896a4d693be4a80388ee9"

testToken :: TokenName 
testToken = TokenName "tDMT"

-- claimTrace :: EmulatorTrace ()
-- claimTrace = do
--   h1 <- activateContractWallet dsWallet endpoints
--   h2 <- activateContractWallet userWallet endpoints
--   callEndpoint @"offer" h1 $ Plutus.Contracts.DropspotDMTDistro.OfferParams {
--     offerTo = stringToBuiltinByteString $ show $ mockWalletPaymentPubKeyHash userWallet,
--     offerQuantity = 42
--   }

--   void $ waitUntilSlot 12
--   callEndpoint @"redeem" h2 ()
--   void $ waitNSlots 1

-- test :: IO ()
-- test = runEmulatorTraceIO' def emCfg claimTrace
--   where
--     emCfg :: EmulatorConfig
--     emCfg = EmulatorConfig { 
--       _initialChainState = Left $ Map.fromList ([(knownWallet i, v) | i <- [2 .. 10]] <> [(knownWallet 1, nftValue)] ),
--       _slotConfig = _slotConfig (def :: EmulatorConfig),
--       _feeConfig = _feeConfig (def :: EmulatorConfig)   
--     }

--     nftValue :: Value
--     nftValue = v <> Value.singleton testCurr testToken 1

--     v :: Value
--     v = Ada.lovelaceValueOf 1_000_000_000


test1 :: Integer -> Integer -> Bool
test1 a b = Value.valueOf v1 testCurr testToken == b -- Value.valueOf v2 testCurr testToken
  where
     
     v1 = Value.singleton testCurr testToken a 
      <> Value.singleton "ff" "T" 1 
      <> Ada.lovelaceValueOf 10_000_000
     
     v2 = Value.singleton testCurr testToken b
      <> Ada.lovelaceValueOf 2_000_000
  
