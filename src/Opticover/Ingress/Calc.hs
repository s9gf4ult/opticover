module Opticover.Ingress.Calc where

import Control.Lens
import Opticover.Geometry
import Opticover.Ingress.Types

link2segment :: Link -> Segment
link2segment = error "FIXME: Not implemented: link2segment"

linksCross :: Link -> Link -> Bool
linksCross a b = link2segment a `segmentCross` link2segment b

-- | Gets two portals as basis and direction of vector respectively
portalVec :: Portal -> Portal -> Vec
portalVec p1 p2 = pointVec (p1 ^. pCoord) (p2 ^. pCoord)
