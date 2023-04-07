module PlutusBuoy.Plutus.Maybe where

import PlutusTx.Prelude

justIf :: (a -> Bool) -> a -> Maybe a
justIf p x = if p x then Just x else Nothing

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x