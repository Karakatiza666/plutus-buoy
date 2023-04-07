module PlutusBuoy.Plutus.Crypto where

import PlutusTx.Prelude
import PlutusTx.Builtins
import PlutusBuoy.Plutus.ByteString

-- Calculate n-byte random number from bytestring seed
{-# INLINABLE drng #-}
drng :: Integer -> BuiltinByteString -> Integer
drng bytes = littleEndianToInteger 256 . sliceByteString (32 - bytes) bytes . blake2b_256

-- Calculate 8-byte random number from bytestring seed
{-# INLINABLE drng8 #-}
drng8 :: BuiltinByteString -> Integer
drng8 = drng 8