{-# LANGUAGE FlexibleContexts #-}

module PlutusBuoy.Plutus.List where

import PlutusTx.Builtins
import PlutusTx.Prelude
import Data.Align
import Data.These
import Data.List (zipWith3)

{-# INLINABLE onlyNAndSatisfy #-}
onlyNAndSatisfy :: Integer -> (a -> Bool) -> [a] -> Maybe [a]
onlyNAndSatisfy n pred list = if length list == n then if pred `all` list then Just list else Nothing else Nothing

{-# INLINABLE onlyAndSatisfies #-}
onlyAndSatisfies :: (a -> Bool) -> [a] -> Maybe a
onlyAndSatisfies pred list = let h = head list in if length list == 1 then if pred h then Just h else Nothing else Nothing

{-# INLINABLE only #-}
only :: [a] -> Maybe a
only [a] = Just a
only _ = Nothing

{-# INLINABLE zipWithDefault #-}
zipWithDefault :: Align f => a -> b -> f a -> f b -> f (a, b)
zipWithDefault da db = alignWith (fromThese da db)

-- Should only be applied to sorted lists
-- Pads both lists with default values so that pairwise element's keys are equal
{-# INLINABLE unzipMatchDefault #-}
unzipMatchDefault :: Ord k => (a -> k) -> (k -> a) -> [a] -> [a] -> ([a], [a])
unzipMatchDefault key dx = match
    where
        dy = dx
        match xs         [] = (xs, (dy . key) `map` xs)
        match []         ys = ((dx . key) `map` ys, ys)
        match ax@(x:xs) ay@(y:ys) = case compare (key x) (key y) of
            EQ -> ($ match xs ys ) \ (as, bs) -> ( x:as,                 y:bs)
            LT -> ($ match xs ay ) \ (as, bs) -> ( x:as,      (dy $ key x):bs)
            GT -> ($ match ax ys ) \ (as, bs) -> ((dx $ key y):as,       y:bs)

{-# INLINABLE zipWithMatchDefault #-}
zipWithMatchDefault :: Ord k => (a -> k) -> (a -> a -> b) -> (k -> a) -> [a] -> [a] -> [b]
zipWithMatchDefault key f dx = match
    where
        dy = dx
        match xs        [] = map (\x -> f x (dy $ key x)) xs -- (ap f (dy . key)) -- (\x -> (x `f` dy (key x)))
        match []        ys = map (f =<< dx . key) ys -- (\y -> dx (key y) `f` y)
        match ax@(x:xs) ay@(y:ys) = case compare (key x) (key y) of
            EQ -> f x             y : match xs ys
            LT -> f x  (dy $ key x) : match xs ay
            GT -> f (dx $ key y)  y : match ax ys

-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list:
--
-- >>> span (< 3) [1,2,3,4,1,2,3,4]
-- ([1,2],[3,4,1,2,3,4])
-- >>> span (< 9) [1,2,3]
-- ([1,2,3],[])
-- >>> span (< 0) [1,2,3]
-- ([],[1,2,3])
--
-- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
{-# INLINABLE span #-}
span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)

{-# INLINABLE splitAtWithLen #-}
splitAtWithLen :: Integer -> [a] -> Integer -> ([a], [a])
splitAtWithLen n xs len
  | n <= 0    = ([], xs)
  | n >= len  = (xs, [])
  | otherwise =
      let (ys, zs) = splitAtWithLen (n-1) (tail xs) (len-1)
      in (head xs : ys, zs)

-- Reduce list with associative function with O(log n)
-- Applies function to sublists recursively, treating list as a tree
{-# INLINABLE reduceAssociative #-}
reduceAssociative :: (a -> a -> a) -> [a] -> a
reduceAssociative _ [] = traceError "reduceAssociative empty list"
reduceAssociative _ [a] = a
reduceAssociative f as =
   let
      len = length as
      (leftLen, _) = sublistLengths len
      (left, right) = splitAtWithLen leftLen as len
   in f (reduceAssociative f left) (reduceAssociative f right)
   where
      sublistLengths :: Integer -> (Integer, Integer)
      sublistLengths len =
         if len <= 1
            then (len, 0)
            else
               let
                  halfLen = len `quotientInteger` 2
                  leftLen = (halfLen + len) `remainderInteger` 2
                  rightLen = halfLen
               in (leftLen, rightLen)

{-# INLINE zipWith3M #-}
zipWith3M :: (Applicative m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M f xs ys zs =  sequenceA (zipWith3 f xs ys zs)

{-# INLINE last #-}
last :: [a] -> a
last [x]                =  x
last (_:xs)             =  last xs
last []                 =  traceError "last: empty"