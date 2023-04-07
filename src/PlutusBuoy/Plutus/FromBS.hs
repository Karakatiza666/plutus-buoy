{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PlutusBuoy.Plutus.FromBS where

import Prelude

import Codec.Serialise
import Data.Semigroup
import Data.ByteString as Strict (ByteString)
import PlutusBuoy.Plutus.Validator
import PlutusBuoy.Plutus.Common
import PlutusBuoy.Plutus.ByteString
import Plutus.V1.Ledger.Scripts

-- import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, textEnvelopeRawCBOR, serialiseToTextEnvelope)
-- import Plutus.Script.Utils.V2.Scripts qualified as ScriptsV2
-- import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as Scripts
-- import PlutusTx
import PlutusTx.Builtins
import Plutus.V1.Ledger.Tx
-- import Plutus.V2.Ledger.Contexts
-- import Plutus.V1.Ledger.Address
-- import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
-- import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Value


class FromBS a where
    fromBS :: Strict.ByteString -> a

instance FromBS PubKeyHash where
    fromBS = PubKeyHash . toBuiltin

instance FromBS CurrencySymbol where
    fromBS = CurrencySymbol . toBuiltin

instance FromBS TokenName where
    fromBS = TokenName . toBuiltin

instance FromBS TxOutRef where
    fromBS bytes' =
        let bytes = toBuiltin bytes'
        in TxOutRef {
            txOutRefId = TxId $ sliceByteString 0 32 $ bytes,
            txOutRefIdx = littleEndianToInteger 256 $ sliceByteString 32 (lengthOfByteString bytes - 32) $ bytes
        }

instance FromBS ScriptHash where
    fromBS = ScriptHash . toBuiltin

-- class SerialiseBS a where
--     serialiseBS :: a -> Strict.ByteString

-- instance SerialiseBS MintingPolicy where
--     serialiseBS = policyHashAndBytes

-- instance SerialiseBS (Scripts.TypedValidator a) where
--     serialiseBS = scriptHashAndBytes