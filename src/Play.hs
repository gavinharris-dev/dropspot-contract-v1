module Play (main) where

import qualified PlutusTx.Prelude

main :: Integer
main = lovelacePercentage 100000000 400

minLovelace :: Integer
minLovelace = 2000000

lovelacePercentage :: Integer -> Integer -> Integer
lovelacePercentage am p =
  if p > 0
    then if result < minLovelace then minLovelace else result
    else 0 -- Prevent Divide By Zero
  where
    result = (am * 10) `PlutusTx.Prelude.divide` p
