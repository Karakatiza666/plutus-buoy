module PlutusBuoy.Plutus.Tuple where

import PlutusTx.Prelude

toFst :: (a -> b) -> a -> (b, a)
toFst f a = (f a, a)
{-# INLINE toFst #-}

toSnd :: (a -> b) -> a -> (a, b)
toSnd f a = (a, f a)
{-# INLINE toSnd #-}

fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst = fmap . toFst
{-# INLINE fmapToFst #-}

fmapToSnd :: Functor f => (a -> b) -> f a -> f (a, b)
fmapToSnd = fmap . toSnd
{-# INLINE fmapToSnd #-}

traverseToFst :: Functor t => (a -> t b) -> a -> t (b, a)
traverseToFst f a = (,a) <$> f a
{-# INLINE traverseToFst #-}

traverseToSnd :: Functor t => (a -> t b) -> a -> t (a, b)
traverseToSnd f a = (a,) <$> f a