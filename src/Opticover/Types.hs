module Opticover.Types where

import Data.List as L
import Data.Text as T

data Point = Point
  { _pX :: Double
  , _pY :: Double
  } deriving (Eq, Ord)

data Portal = Portal
  { _pCoord :: Point
  , _pName :: Text
  } deriving (Eq, Ord)

data PleOrd = PleOrd | PleUnord
  deriving (Eq, Ord)

data Pair (ord :: PleOrd) a = Pair a a

instance (Ord a) => Eq (Pair 'PleUnord a) where
  (Pair a b) == (Pair c d) = L.sort [a, b] == L.sort [c, d]

instance (Ord a) => Ord (Pair 'PleUnord a) where
  compare (Pair a b) (Pair c d) =
    compare (L.sort [a, b]) (L.sort [c, d])

data Triple (ord :: PleOrd) a = Triple a a a

instance (Ord a) => Eq (Triple 'PleUnord a) where
  Triple a b c == Triple x y z =
    L.sort [a, b, c] == L.sort [x, y, z]

instance (Ord a) => Ord (Triple 'PleUnord a) where
  compare (Triple a b c) (Triple x y z) =
    compare (L.sort [a, b, c]) (L.sort [x, y, z])

-- | The link is unordered pair of portals
newtype Link = Link
  { unLink :: Pair 'PleUnord Portal
  } deriving (Eq, Ord)

-- | The field is unordered triple of links
newtype Field = Field
  { unField :: Triple 'PleUnord Field
  } deriving (Eq, Ord)
