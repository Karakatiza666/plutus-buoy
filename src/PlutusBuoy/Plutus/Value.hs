{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-specialise #-}

module PlutusBuoy.Plutus.Value where

import Ledger.Value
import PlutusTx.Prelude
import PlutusTx.AssocMap qualified as PMap

{-# INLINABLE singletonValueTokens #-}
singletonValueTokens :: Value -> [(TokenName, Integer)]
singletonValueTokens value = case PMap.toList $ getValue value of
   [(_, v)] -> PMap.toList v
   _ -> traceError "singletonValueTokens: not a singleton"