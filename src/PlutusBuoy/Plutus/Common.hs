{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-specialise #-}

module PlutusBuoy.Plutus.Common where

import Codec.Serialise
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, textEnvelopeRawCBOR, serialiseToTextEnvelope)
import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.ByteString.Short qualified as SBS
import Data.ByteString.Lazy qualified as Lazy
import Data.ByteString qualified as Strict
import Data.ByteString.Base16.Lazy as Lazy16
import Data.Default (Default (def))
import Data.List.NonEmpty (nonEmpty, NonEmpty((:|)))
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Coerce (coerce)

import PlutusTx
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Scripts

-- import Ledger.Ada
import Ledger.Interval qualified as Interval
-- import Plutus.V1.Ledger.Api
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx qualified as Tx
-- import Plutus.Script.Utils.V1.Scripts (MintingPolicy)
import Plutus.Script.Utils.V2.Scripts qualified as ScriptsV2
import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as Scripts
import Ledger.Value -- (Value)
import Ledger.Value qualified as Value
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.Test hiding (not)
-- import Plutus.Contract.Typed.Tx qualified as Typed
import PlutusTx qualified
-- import PlutusTx.Prelude hiding (Semigroup (..), fold, foldr)
import PlutusTx.Prelude hiding (fold, foldr)
import PlutusTx.Prelude qualified as PP
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Builtins hiding (foldr)
import PlutusTx.Foldable (foldl, foldr)
import PlutusTx.Numeric ()
import PlutusTx.Maybe (maybe)
-- import Data.Semigroup (Sum (Sum), getSum)

import Prelude qualified as Haskell
import Schema (ToSchema)
import Control.Arrow ((&&&))

-- {-# INLINABLE takeExactMap #-}
-- takeExactMap :: Integer -> Map k a -> Maybe (Map k a)
-- takeExactMap n m = let h = Map.take (fromIntegral n) m in if toInteger (Map.size h) == n then Just h else Nothing

{-# INLINABLE first3M #-}
first3M :: Functor f => (a -> f a') -> (a,b,c) -> f (a',b,c)
first3M f (a,b,c) = fmap (,b,c) $ f a

{-# INLINABLE second3M #-}
second3M :: Functor f => (b -> f b') -> (a,b,c) -> f (a,b',c)
second3M f (a,b,c) = fmap (a,,c) $ f b

{-# INLINABLE third3M #-}
third3M :: Functor f => (c -> f c') -> (a,b,c) -> f (a,b,c')
third3M f (a,b,c) = fmap (a,b,) $ f c

{-# INLINABLE foldMap_ #-}
foldMap_ :: Monoid m => (a -> m) -> [a] -> m
-- foldMap_ :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldMap_ = foldMap
foldMap_ _ []     = mempty
foldMap_ f (x:xs) = f x <> foldMap_ f xs
-- foldMap_ f list = if null list then mempty else (\(x:xs) -> f x PP.<> foldMap_ f xs) list
-- foldMap_ f list = mempty
-- foldMap_ f = foldr (mappend . f) mempty

--{-# INLINABLE fromJust #-}
-- fromJust Nothing  = fix id -- traceError "Maybe.fromJust: Nothing" -- yuck
--     where
--         fix :: (a -> a) -> a
--         fix f = let x = f x in x
-- fromJust (Just x) = x

{-# INLINABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just a) = a

{-# INLINABLE outIdxHash #-}
outIdxHash :: Tx.TxOutRef -> BuiltinByteString
outIdxHash Tx.TxOutRef {..} = consByteString txOutRefIdx $ sliceByteString 0 31 $ getTxId txOutRefId

{-# INLINABLE outTxId #-}
outTxId :: Tx.TxOutRef -> BuiltinByteString
outTxId = getTxId . txOutRefId

-- {-# INLINABLE getPolicyTokens #-}
-- getPolicyTokens policy = concat . fmap PMap.toList . PMap.lookup policy . Value.getValue


-- newtype Any = Any { getAny :: Bool }
--         deriving ( Eq      -- ^ @since 2.01
--                  , Ord     -- ^ @since 2.01
--                  , Show    -- ^ @since 2.01
--                  , Generic -- ^ @since 4.7.0.0
--                  )
-- 
-- -- | @since 4.9.0.0
-- instance PP.Semigroup Any where
--         (<>) = coerce (||)
-- 
-- -- | @since 2.01
-- instance Monoid Any where
--         mempty = Any False
-- 
-- any :: Foldable t => (a -> Bool) -> t a -> Bool
-- any p = getAny . foldMap (Any . p)

{-# INLINABLE biliftA2 #-}
biliftA2 :: (a -> b -> c) -> (a' -> b' -> c') -> (a, a') -> (b, b') -> (c, c')
h `biliftA2` h' = \ (a, a') (b, b') -> (h a b, h' a' b')

{-# INLINABLE on' #-}
-- redefine on so we avoid duplicate computation for most values.
on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on'` f = \x -> let fx = f x in \y -> fx .*. f y

{-# INLINABLE groupOn #-}
groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f = map snd . foldr go [] . map (f &&& id)
    where
        go (k, x) ((k', xs) : ys) | k == k' = (k, x:xs) : ys
        go (k, x) kxs = (k, [x]) : kxs

{-# INLINABLE maybeOnly #-}
maybeOnly :: [a] -> Maybe a
maybeOnly [a] = Just a
maybeOnly _ = Nothing

{-# INLINABLE ensure #-}
ensure :: (a -> Bool) -> a -> Maybe a
ensure p a
  | p a       = Just a
  | otherwise = Nothing

{-# INLINABLE foldl1 #-}
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
foldl1 f xs = fromMaybeTraceError "foldl1: empty structure" (foldl mf Nothing xs)
    where
        mf m y = Just $ maybe y (`f` y) m

-- 
{-# INLINABLE foldlNonEmpty #-}
foldlNonEmpty :: (b -> a -> b) -> b -> [a] -> Maybe b
foldlNonEmpty _ _ [] = Nothing
foldlNonEmpty f a as = Just $ foldl f a as

{-# INLINABLE swap #-}
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

{-# INLINABLE if' #-}
if' :: Bool -> a -> a -> a
if' cond true false = if cond then true else false

{-# INLINABLE flap #-}
flap :: Functor f => f (a -> b) -> a -> f b
flap f x = fmap ($ x) f

infixl 4 <@>
{-# INLINABLE (<@>) #-}
(<@>) :: Functor f => f (a -> b) -> a -> f b
(<@>) = flap

{-# INLINABLE valueOf' #-}
valueOf' :: CurrencySymbol -> TokenName -> Value -> Integer
valueOf' p a v = Value.valueOf v p a

{-# INLINABLE onlyValueOf #-}
onlyValueOf :: CurrencySymbol -> TokenName -> Value -> Value
onlyValueOf symbol token = Value.singleton symbol token . valueOf' symbol token

{-# INLINABLE zeroValue #-}
zeroValue :: Value
zeroValue = Value.singleton adaSymbol adaToken 0

{-# INLINABLE emptyTokenName #-}
emptyTokenName :: TokenName
emptyTokenName = TokenName emptyByteString

{-# INLINABLE maybeTraceError #-}
maybeTraceError str _ Nothing = traceError str
maybeTraceError _ f (Just a) = f a

{-# INLINABLE fromMaybeTraceError #-}
fromMaybeTraceError str Nothing = traceError str
fromMaybeTraceError _ (Just a) = a

-- newtype Sum a = Sum { getSum :: a }
--         deriving ( Eq
--                  , Ord
--                  , Show
--                  , Num 
--                  )

-- -- instance Num a => Semigroup (Sum a) where
-- --     (<>) = coerce ((+) :: a -> a -> a)

-- -- instance Num a => Monoid (Sum a) where
-- --     mempty = Sum 0
-- instance Semigroup (Sum Integer) where
--     {-# INLINABLE (<>) #-}
--     (<>) = (+)
-- instance Monoid (Sum Integer) where
--     {-# INLINABLE mempty #-}
--     mempty = Sum 0

{-# INLINABLE valueAssetsQty #-}
-- Including ADA
valueAssetsQty :: Value -> Integer
-- TODO: optimize
-- valueAssetsQty = foldr ((+) . foldr ((+) . const 1) 0) 0 . Value.getValue -- uses non Plutus Core functions
-- valueAssetsQty = getSum . foldMap (foldMap (const $ Sum 1)) . Value.getValue -- uses non Plutus Core functions
valueAssetsQty = length . flattenValue

-- (IsPlutusScriptLanguage lang, Typeable lang) => HasTextEnvelope (PlutusScript lang)


{-# INLINABLE hasCurrency #-}
hasCurrency :: CurrencySymbol -> Value -> Bool
hasCurrency currency = isJust . PMap.lookup currency . getValue

{-# INLINABLE txOutHasCurrency #-}
txOutHasCurrency :: CurrencySymbol -> TxOut -> Bool
txOutHasCurrency currency = hasCurrency currency . txOutValue

{-# INLINABLE txInHasCurrency #-}
txInHasCurrency :: CurrencySymbol -> TxInInfo -> Bool
txInHasCurrency currency = hasCurrency currency . txOutValue . txInInfoResolved

{-# INLINABLE hasToken #-}
hasToken :: CurrencySymbol -> TokenName -> Value -> Bool
hasToken currency name = isJust . (PMap.lookup name <=< PMap.lookup currency) . getValue

{-# INLINABLE txOutHasToken #-}
txOutHasToken :: CurrencySymbol -> TokenName -> TxOut -> Bool
txOutHasToken currency name = hasToken currency name . txOutValue

{-# INLINABLE txInHasToken #-}
txInHasToken :: CurrencySymbol -> TokenName -> TxInInfo -> Bool
txInHasToken currency name = hasToken currency name . txOutValue . txInInfoResolved

{-# INLINABLE findCurrency #-}
findCurrency :: CurrencySymbol -> Value -> Maybe (PMap.Map TokenName Integer)
findCurrency currency = PMap.lookup currency . getValue

{-# INLINABLE flattenCurrency #-}
flattenCurrency :: CurrencySymbol -> Value -> [(TokenName, Integer)]
flattenCurrency currency = maybe [] PMap.toList . PMap.lookup currency . getValue

{-# INLINABLE spentValue #-}
spentValue :: TxInfo -> (Value -> Bool) -> Bool
spentValue txIn pred = (pred . txOutValue . txInInfoResolved) `any` txInfoInputs txIn

{-# INLINABLE txSignedByPubKey #-}
txSignedByPubKey :: TxInfo -> Address -> Bool
txSignedByPubKey txIn Address { addressCredential = PubKeyCredential hash } = txSignedBy txIn hash
txSignedByPubKey _ _ = False

-- Spent or referenced from signing PubKeyHash
{-# INLINABLE spentOrReferencedValue #-}
spentOrReferencedValue :: TxInfo -> (Value -> Bool) -> Bool
spentOrReferencedValue txIn pred = if spentValue txIn pred then True else referenced
    where
        referenced = (witnessed . txInInfoResolved) `any` txInfoReferenceInputs txIn
        witnessed txOut = if pred $ txOutValue $ txOut
            then txSignedByPubKey txIn $ txOutAddress txOut
            else False

-- =====================



-- =====================

-- bytesToPubKeyHash = PubKeyHash . toBuiltin . toStrict
-- bytesToCurrencySymbol = CurrencySymbol . toBuiltin . toStrict
-- bytesToTokenName = TokenName . toBuiltin . toStrict

-- Is implemented in latest PlutusTx.Prelude!
-- {-# INLINABLE sort #-}
-- -- | Plutus Tx version of 'Data.List.sort'.
-- sort :: Ord a => [a] -> [a]
-- sort = sortBy compare

-- {-# INLINABLE sortBy #-}
-- -- | Plutus Tx version of 'Data.List.sortBy'.
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
-- sortBy cmp l = mergeAll (sequences l)
--   where
--     sequences (a:b:xs)
--       | a `cmp` b == GT = descending b [a]  xs
--       | otherwise       = ascending  b (a:) xs
--     sequences xs = [xs]

--     descending a as (b:bs)
--       | a `cmp` b == GT = descending b (a:as) bs
--     descending a as bs  = (a:as): sequences bs

--     ascending a as (b:bs)
--       | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
--     ascending a as bs   = let x = as [a]
--                           in x : sequences bs

--     mergeAll [x] = x
--     mergeAll xs  = mergeAll (mergePairs xs)

--     mergePairs (a:b:xs) = let x = merge a b
--                           in x : mergePairs xs
--     mergePairs xs       = xs

--     merge as@(a:as') bs@(b:bs')
--       | a `cmp` b == GT = b:merge as  bs'
--       | otherwise       = a:merge as' bs
--     merge [] bs         = bs
--     merge as []         = as