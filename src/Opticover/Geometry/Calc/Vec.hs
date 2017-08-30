module Opticover.Geometry.Calc.Vec where

import Control.Lens
import Data.AEq
import Opticover.Geometry.Types
import Opticover.Ple

vecFromPoints
  :: Point
  -- ^ Vector origin
  -> Point
  -- ^ Vector destination
  -> Vec
vecFromPoints (Point bx by) (Point dx dy) = Vec $ Point (dx - bx) (dy - by)

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
vecSquare :: Vec -> Vec -> Double
vecSquare v1 v2 = abs $ (vecPerpProduct v1 v2) / 2

vecPerp :: Vec -> Vec
vecPerp (Vec (Point x y)) = Vec $ Point (negate y) x

vecCoords :: Vec -> [Double]
vecCoords (Vec (Point x y)) = [x, y]

vecScalarProduct :: Vec -> Vec -> Double
vecScalarProduct v1 v2 = sum $ zipWith (*) (vecCoords v1) (vecCoords v2)

vecPerpProduct :: Vec -> Vec -> Double
vecPerpProduct v1 v2 = vecScalarProduct (vecPerp v1) v2
