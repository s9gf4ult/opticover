module Opticover.Geometry.Types where

import Control.Lens
import Opticover.Ple

data Point = Point
  { _pX :: Double
  , _pY :: Double
  } deriving (Eq, Ord, Show)

makeLenses ''Point

-- | Box is an ordered pair of 2 points: low left corner and top right
-- corner respectively
newtype Box = Box (Pair Point)
  deriving (Eq, Ord, Show)

makePrisms ''Box

newtype Vec = Vec
  { unVec :: Point
  } deriving (Eq, Ord, Show)

makePrisms ''Vec

-- | Line in parametric coordinates
newtype Line = Line
  { unLine :: (Pair Point)
  } deriving (Eq, Ord, Show)

makePrisms ''Line

newtype Segment = Segment
  { unSegment :: (Pair Point)
  } deriving (Eq, Ord, Show)

makePrisms ''Segment


newtype Triangle = Triangle
  { unTriangle :: Triple Point
  } deriving (Eq, Ord, Show)

makePrisms ''Triangle
