module Cardano.Plutus.ByteString where

import PlutusTx.Prelude
-- import PlutusTx.Builtins

{-# INLINABLE foldrByteString #-}
foldrByteString :: (Integer -> b -> b) -> b -> BuiltinByteString -> b
foldrByteString f start bytes = loop $ lengthOfByteString bytes
    where
        loop n = let m = n - 1 in f (indexByteString bytes m) (if m == 0 then start else loop m)

{-# INLINABLE foldlByteString #-}
foldlByteString :: (b -> Integer -> b) -> b -> BuiltinByteString -> b
foldlByteString f start bytes = loop 0 start
    where
        len = lengthOfByteString bytes
        loop i acc = if i == len
          then acc
          else loop (i + 1) (f acc (indexByteString bytes i))

{-# INLINEABLE integerToDecBytes #-}
integerToDecBytes :: Integer -> BuiltinByteString
integerToDecBytes num = iterate num emptyByteString
    where
        base = 10
        iterate num
          | num < 0 = consByteString 45 . iterate (negate num)
          | quot < base = consByteString digit
          | otherwise = iterate quot . consByteString digit
            where
                quot = num `quotient` base
                rem = num `remainder` base
                digit = rem + 48

{-# INLINEABLE snocByteString #-}
snocByteString :: BuiltinByteString -> Integer -> BuiltinByteString
snocByteString str byte = appendByteString str (consByteString byte emptyByteString)

{-# INLINEABLE integerToBigEndian #-}
integerToBigEndian :: Integer -> Integer -> BuiltinByteString
integerToBigEndian base num = result
  where
    (result, _) = iterate num emptyByteString
    iterate 0 acc = (acc, 0)
    iterate n acc =
      let
        (q, r) = n `quotRem` base
        acc' = consByteString r acc
      in iterate q acc'

{-# INLINEABLE integerToLittleEndian #-}
integerToLittleEndian :: Integer -> Integer -> BuiltinByteString
integerToLittleEndian base num = result
  where
    (result, _) = iterate num emptyByteString
    iterate 0 acc = (acc, 0)
    iterate n acc =
      let
        (q, r) = n `quotRem` base
        acc' = snocByteString acc r
      in iterate q acc'

{-# INLINABLE bigEndianToInteger #-}
bigEndianToInteger :: Integer -> BuiltinByteString -> Integer
bigEndianToInteger base = foldrByteString (\cur acc -> acc * base + cur) 0

{-# INLINABLE littleEndianToInteger #-}
littleEndianToInteger :: Integer -> BuiltinByteString -> Integer
littleEndianToInteger base = foldlByteString (\acc cur -> acc * base + cur) 0

-- encodeHex :: BuiltinByteString -> BuiltinByteString
-- encodeHex = foldr (consHex . hexOf) empty
--     where
--         consHex (a, b) = consByteString a . consByteString b
--         hexOf n = let (d, m) = n `divMod` 16 in (hexSymbol d, hexSymbol m)
--         hexSymbol b = if b < 10 then b + 48 else b + 87