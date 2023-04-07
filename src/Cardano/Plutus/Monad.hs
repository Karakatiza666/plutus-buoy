
module Cardano.Plutus.Monad where

import PlutusTx.Prelude
import Control.Monad

join              :: (Monad m) => m (m a) -> m a
join x            =  x >>= id