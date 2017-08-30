module Opticover.Geometry.Calc.Triangle where

import Control.Lens
import Data.AEq
import Opticover.Geometry.Calc.Vec
import Opticover.Geometry.Types
import Opticover.Ple

triangleSquare :: Triangle -> Double
triangleSquare t =
  let (a, b, c) = unTriple $ unTriangle t
      v1 = vecFromPoints a b
      v2 = vecFromPoints a c
  in vectorsSquare v1 v2

pointInTriangle :: Triangle -> Point -> Bool
pointInTriangle = error "Not implemented: pointInTriangle"
