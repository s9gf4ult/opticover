-- | Geometry calculations and stuff
module Opticover.Ingress.Game.Calc
  ( -- * Geometry conversions
    link2segment
  , portalVec
  , fieldToTriangle
    -- * Checks
  , linksCross
  , portalUnderField
  ) where

import Control.Lens
import Data.Set as S
import Opticover.Geometry
import Opticover.Ingress.Game.Types
import Opticover.Ple

link2segment :: Link -> Segment
link2segment l =
  let Link p1 p2 = l
  in Segment $ unordPair (p1 ^. pCoord) (p2 ^. pCoord)

-- | Gets two portals as basis and direction of vector respectively
portalVec :: Portal -> Portal -> Vec
portalVec p1 p2 = vecFromPoints (p1 ^. pCoord) (p2 ^. pCoord)

fieldToTriangle :: Field -> Triangle
fieldToTriangle f =
  let
    (a, b, c) = unTriple $ fieldPortals f
    t = Triangle $ unordTriple (a ^. pCoord) (b ^. pCoord) (c ^. pCoord)
  in t

linksCross :: Link -> Link -> Bool
linksCross a b = link2segment a `segmentCross` link2segment b

portalUnderField :: Portal -> Field -> Bool
portalUnderField prt f =
  let t = fieldToTriangle f
      p = prt ^. pCoord
  in pointInTriangle t p
