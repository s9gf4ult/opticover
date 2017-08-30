module Opticover.Geometry.Calc.Box where

import Control.Lens
import Data.AEq
import Opticover.Geometry.Types
import Opticover.Ple

pointInBox :: Box -> Point -> Bool
pointInBox (Box pair) p =
  let (p1, p2) = unPair pair
  in p1 <= p && p <= p2
  -- Hoping derived Ord instance do what we expect here

segmentBorderBox :: Segment -> Box
segmentBorderBox (Segment pair) = Box pair
