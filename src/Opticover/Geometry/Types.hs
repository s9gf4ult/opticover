-- |

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

data Line = Line
  { _lBase      :: !Point
  , _lDirection :: !Vec
  -- ^ Direction vector, the length is not relevant, should be
  -- normalized to 1.
  } deriving (Eq, Ord, Show)

makeLenses ''Line

newtype Segment = Segment (Pair Point)
  deriving (Eq, Ord, Show)

makePrisms ''Segment
