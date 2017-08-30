module Opticover.Geometry.Calc.Triangle where

import Control.Lens
import Control.Monad
import Data.AEq
import Data.Maybe
import Opticover.Geometry.Calc.Line
import Opticover.Geometry.Calc.Vec
import Opticover.Geometry.Types
import Opticover.Ple

triangleSquare :: Triangle -> Double
triangleSquare t =
  let (a, b, c) = unTriple $ unTriangle t
      v1 = vecFromPoints a b
      v2 = vecFromPoints a c
  in vecSquare v1 v2

pointInTriangle :: Triangle -> Point -> Bool
pointInTriangle t p =
  let
    (a, b, c) = unTriple $ unTriangle t
    abVec = vecFromPoints a b
    acVec = vecFromPoints a c
    apVec = vecFromPoints a p
    res = do
      let
        tPerp = vecPerpProduct abVec acVec
        pPerp = vecPerpProduct abVec apVec
      guard $ (tPerp > 0 && pPerp > 0) || (tPerp < 0 && pPerp < 0)
      -- lie at same side from triangle edge
      let
        acSeg = Segment $ unordPair a c
        cbSeg = Segment $ unordPair c b
        apSeg = Segment $ unordPair a p
        pbSeg = Segment $ unordPair p b
      guard $ not $ segmentCross acSeg pbSeg
      guard $ not $ segmentCross cbSeg apSeg
      guard $ vecSquare abVec acVec > vecSquare abVec apVec
      return True
  in fromMaybe False res
