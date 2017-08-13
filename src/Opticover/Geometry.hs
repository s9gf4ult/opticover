module Opticover.Geometry where

import Opticover.Types
import Control.Lens

linksCross :: Link -> Link -> Bool
linksCross a b = (error "FIXME: linksCross")

pointVec :: Point -> Point -> Vec
pointVec (Point bx by) (Point dx dy) = Vec $ Point (dx - bx) (dy - by)

-- | Gets two portals as basis and direction of vector respectively
portalVec :: Portal -> Portal -> Vec
portalVec p1 p2 = pointVec (p1 ^. pCoord) (p2 ^. pCoord)

vecLen :: Vec -> Double
vecLen (Vec (Point x y)) = sqrt $ (x ^ 2) + (y ^ 2)

-- | Sin of angle between two vectors
vecAngleSin :: Vec -> Vec -> Double
vecAngleSin v1 v2 = vecPerpProduct v1 v2 / (vecLen v1) / (vecLen v2)

-- | Calculates square of triangle formed by two vectors
vectorsSquare :: Vec -> Vec -> Double
vectorsSquare v1 v2 = abs $ (vecLen v1) * (vecLen v2) * (vecAngleSin v1 v2) / 2

vecPerp :: Vec -> Vec
vecPerp (Vec (Point x y)) = Vec $ Point (negate y) x

vecCoords :: Vec -> [Double]
vecCoords (Vec (Point x y)) = [x, y]

vecScalarProduct :: Vec -> Vec -> Double
vecScalarProduct v1 v2 = sum $ zipWith (*) (vecCoords v1) (vecCoords v2)

vecPerpProduct :: Vec -> Vec -> Double
vecPerpProduct v1 v2 = vecScalarProduct (vecPerp v1) v2
