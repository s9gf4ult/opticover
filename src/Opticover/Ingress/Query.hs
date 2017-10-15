module Opticover.Ingress.Query where

import Control.Lens
import Control.Monad
import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Set as S
import Opticover.Geometry
import Opticover.Ingress.Types
import Opticover.Ple


leftRightSplit :: Link -> [Portal] -> ([Portal], [Portal])
leftRightSplit l portals = L.partition leftPortal portals
  where
    leftPortal p =
      let (a, b) = unPair $ unLink l
          vec1 = portalVec a b
          vec2 = portalVec a p
      in vecPerpProduct vec1 vec2 > 0

fieldSquare :: Field -> Double
fieldSquare field =
  let
    (a, b, c) = unTriple $ fieldPortals field
    vec1 = portalVec a b
    vec2 = portalVec a c
  in vecSquare vec1 vec2
