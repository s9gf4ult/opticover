module Opticover.Ingress.Calc where

import Control.Lens
import Data.Set as S
import Opticover.Geometry
import Opticover.Ingress.Types
import Opticover.Ple

link2segment :: Link -> Segment
link2segment l =
  let (p1, p2) = unPair $ unLink l
  in Segment $ unordPair (p1 ^. pCoord) (p2 ^. pCoord)

linksCross :: Link -> Link -> Bool
linksCross a b = link2segment a `segmentCross` link2segment b

-- | Gets two portals as basis and direction of vector respectively
portalVec :: Portal -> Portal -> Vec
portalVec p1 p2 = vecFromPoints (p1 ^. pCoord) (p2 ^. pCoord)

fieldToTriangle :: Field -> Maybe Triangle
fieldToTriangle f = do
  [a, b, c] <- pure $ fieldPortals f ^.. folded . pCoord
  let t = Triangle $ unordTriple a b c
  return t
