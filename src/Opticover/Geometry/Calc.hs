module Opticover.Geometry.Calc where

import Opticover.Geometry.Types
import Control.Lens


-- | Returns Nothing if lines are parallel
lineCrossPoint :: Line -> Line -> Maybe Point
lineCrossPoint = (error "FIXME: ")

segment2line :: Segment -> Line
segment2line (Segment orig dest) = Line orig (pointVec orig dest)

pointInBox :: Box -> Point -> Bool
pointInBox = error "FIXME: Not implemented: pointInBox"

segmentBorderBox :: Segment -> Box
segmentBorderBox = error "FIXME: Not implemented: segmentBorderBox"

segmentCross :: Segment -> Segment -> Bool
segmentCross s1 s2 = case segment2line s1 `lineCrossPoint` segment2line s2 of
  Nothing -> False
  Just p -> pointInBox (segmentBorderBox s1) p
    && pointInBox (segmentBorderBox s2) p

pointVec
  :: Point
  -- ^ Vector origin
  -> Point
  -- ^ Vector destination
  -> Vec
pointVec (Point bx by) (Point dx dy) = Vec $ Point (dx - bx) (dy - by)


vecLen :: Vec -> Double
vecLen (Vec (Point x y)) = sqrt $ (x ^ 2) + (y ^ 2)

-- | Sin of angle between two vectors
vecAngleSin :: Vec -> Vec -> Double
vecAngleSin v1 v2 = vecPerpProduct v1 v2 / (vecLen v1) / (vecLen v2)

-- | Calculates square of triangle formed by two vectors
vectorsSquare :: Vec -> Vec -> Double
vectorsSquare v1 v2 = abs $ (vecPerpProduct v1 v2) / 2

vecPerp :: Vec -> Vec
vecPerp (Vec (Point x y)) = Vec $ Point (negate y) x

vecCoords :: Vec -> [Double]
vecCoords (Vec (Point x y)) = [x, y]

vecScalarProduct :: Vec -> Vec -> Double
vecScalarProduct v1 v2 = sum $ zipWith (*) (vecCoords v1) (vecCoords v2)

vecPerpProduct :: Vec -> Vec -> Double
vecPerpProduct v1 v2 = vecScalarProduct (vecPerp v1) v2
