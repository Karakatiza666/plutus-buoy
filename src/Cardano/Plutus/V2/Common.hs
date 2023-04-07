
module Cardano.Plutus.V2.Common where

import PlutusTx
import PlutusTx.Prelude
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Scripts
import Cardano.Plutus.Common

{-# INLINABLE fromDatum #-}
fromDatum :: PlutusTx.FromData a => Datum -> Maybe a
fromDatum = PlutusTx.fromBuiltinData . getDatum

{-# INLINABLE fromRedeemer #-}
fromRedeemer :: PlutusTx.FromData a => Redeemer -> Maybe a
fromRedeemer = PlutusTx.fromBuiltinData . getRedeemer

-- Throws if couldn't read datum hash or parse datum
{-# INLINABLE lookupDatum #-}
lookupDatum :: PlutusTx.FromData a => TxInfo -> OutputDatum -> Maybe a
lookupDatum _ NoOutputDatum = Nothing
lookupDatum txIn (OutputDatumHash dh) = maybeTraceError "Failed to find hash of or decode datum" Just $ fromDatum =<< flip findDatum txIn dh
lookupDatum _ (OutputDatum d) = maybeTraceError "Failed to find hash of or decode datum" Just $ fromDatum d

readTxOutDatum :: PlutusTx.FromData a => TxInfo -> TxOut -> Maybe a
readTxOutDatum txIn TxOut{txOutAddress = Address{addressCredential=ScriptCredential _}, ..} =
    lookupDatum txIn txOutDatum
readTxOutDatum _ _ = traceError "Non-script output cannot have a datum"
