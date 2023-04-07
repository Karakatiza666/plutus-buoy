module PlutusBuoy.Plutus.V2.ToBBS where

import PlutusTx.Prelude
import PlutusTx.Builtins
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api
import PlutusBuoy.Plutus.ByteString

class ToBBS a where
   toBBS :: a -> BuiltinByteString

instance ToBBS TxOutRef where
   {-# INLINABLE toBBS #-}
   toBBS TxOutRef { .. } =
      getTxId txOutRefId <> integerToLittleEndian 256 txOutRefIdx