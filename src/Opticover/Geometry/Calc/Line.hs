module Opticover.Geometry.Calc.Line where

import Control.Lens
import Data.AEq
import Opticover.Geometry.Calc.Box
import Opticover.Geometry.Calc.Vec
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
      det4 a b c d = (a*d) - (b*c)
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
  in vecFromPoints a b

segment2line :: Segment -> Line
segment2line (Segment pair) = Line pair

segmentCross :: Segment -> Segment -> Bool
segmentCross s1 s2 = case segment2line s1 `lineCrossPoint` segment2line s2 of
  Nothing -> False
  Just p -> pointInBox (segmentBorderBox s1) p
    && pointInBox (segmentBorderBox s2) p
