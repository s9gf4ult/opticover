module Opticover.Geometry.Calc where

import Control.Lens
import Data.AEq
import Numeric.LinearAlgebra
import Opticover.Geometry.Types
import Opticover.Ple

-- | Returns Nothing if lines are parallel
lineCrossPoint :: Line -> Line -> Maybe Point
lineCrossPoint l1 l2 = if
  | pp ~== 0 -> Nothing
    -- Lines are parallel
  | otherwise ->
    let
      (p1, p2) = unPair $ unLine l1
      (p3, p4) = unPair $ unLine l2
      det4 a b c d = det $ (2 >< 2) [a, b, c, d]
      divider =
        let
          a = det4 (p1 ^. pX) 1
                   (p2 ^. pX) 1
          b = det4 (p1 ^. pY) 1
                   (p2 ^. pY) 1
          c = det4 (p3 ^. pX) 1
                   (p4 ^. pX) 1
          d = det4 (p3 ^. pY) 1
                   (p4 ^. pY) 1
        in det4 a b c d
      -- l is either pX or pX
      denomLen l =
        let
          a = det4 (p1 ^. pX) (p1 ^. pY)
                   (p2 ^. pX) (p2 ^. pY)
          b = det4 (p1 ^. l) 1
                   (p2 ^. l) 1
          c = det4 (p3 ^. pX) (p3 ^. pY)
                   (p4 ^. pX) (p4 ^. pY)
          d = det4 (p3 ^. l) 1
                   (p4 ^. l) 1
        in det4 a b c d
      denomX = denomLen pX
      denomY = denomLen pY
    in Just $ Point (denomX / divider) (denomY / divider)
  where
    pp =
      let vec1 = lineDirVec l1
          vec2 = lineDirVec l2
      in vecPerpProduct vec1 vec2

lineDirVec :: Line -> Vec
lineDirVec l =
  let (a, b) = unPair $ unLine l
  in pointVec a b

segment2line :: Segment -> Line
segment2line (Segment pair) = Line pair

pointInBox :: Box -> Point -> Bool
pointInBox (Box pair) p =
  let (p1, p2) = unPair pair
  in p1 <= p && p <= p2
  -- Hoping derived Ord instance do what we expect here

segmentBorderBox :: Segment -> Box
segmentBorderBox (Segment pair) = Box pair

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

vecScale :: Vec -> Double -> Vec
vecScale (Vec (Point a b)) scale = Vec $ Point (a * scale) (b * scale)

-- | Normalizes vector to length 1
vecNormalize :: Vec -> Vec
vecNormalize v = vecScale v (1 / vecLen v)

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
