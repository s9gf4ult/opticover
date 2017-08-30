module Opticover.Geometry.Calc.Triangle where

import Control.Monad
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

-- | If point lies on edge or match one of vertexes then it considered
-- to be in triangle
pointInTriangle :: Triangle -> Point -> Bool
pointInTriangle t p =
  let
    (a, b, c) = unTriple $ unTriangle t
    abVec = vecFromPoints a b
    acVec = vecFromPoints a c
    apVec = vecFromPoints a p
    onPoints = p `elem` [a, b, c]
    insideTriangle = fromMaybe False $ do
      let
        tPerp = vecPerpProduct abVec acVec
        pPerp = vecPerpProduct abVec apVec
        abSeg = Segment $ unordPair a b
      guard $ (tPerp > 0 && pPerp > 0) || (tPerp < 0 && pPerp < 0)
        || pointOnSegment abSeg p
      -- lie at same side from triangle edge
      let
        acSeg = Segment $ unordPair a c
        cbSeg = Segment $ unordPair c b
        apSeg = Segment $ unordPair a p
        pbSeg = Segment $ unordPair p b
      guard $ not
        $ segmentCross acSeg pbSeg && not (pointOnSegment acSeg p)
      guard $ not
        $ segmentCross cbSeg apSeg && not (pointOnSegment cbSeg p)
      guard $ vecSquare abVec acVec >= vecSquare abVec apVec
      return True
  in onPoints || insideTriangle
