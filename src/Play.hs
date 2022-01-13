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


module Play (main) where

import GHC.Generics
import PlutusTx;
import Data.Aeson.Types (ToJSON, FromJSON)
import Data.Aeson (toJSON)
import Prelude (Integer, String, Show (show))


data MarketRedeemer = Buy | Cancel

PlutusTx.makeIsDataIndexed ''MarketRedeemer [ ('Buy, 0), ('Cancel, 1)]
-- PlutusTx.unstableMakeIsData ''MarketRedeemer
PlutusTx.makeLift ''MarketRedeemer

main :: Data
main = PlutusTx.toData (Buy)