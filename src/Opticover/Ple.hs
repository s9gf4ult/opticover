-- | Unordered N-tuples

module Opticover.Ple
  ( Pair
  , unPair
  , unordPair
  , Triple
  , unTriple
  , unordTriple
  ) where

import Data.List as L

newtype Pair a = Pair
  { unPair :: (a, a)
  } deriving (Eq, Ord)

unordPair :: (Ord a) => a -> a -> Pair a
unordPair a b =
  let [x,y] = L.sort [a,b]
  in Pair (x, y)

newtype Triple a = Triple
  { unTriple :: (a, a, a)
  } deriving (Eq, Ord)

unordTriple :: (Ord a) => a -> a -> a -> Triple a
unordTriple a b c =
  let [x, y, z] = L.sort [a, b, c]
  in Triple (x, y, z)
