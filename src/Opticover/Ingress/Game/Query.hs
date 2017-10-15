module Opticover.Ingress.Game.Query where

import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import Opticover.Import
import Opticover.Ingress.Game.Calc
import Opticover.Ingress.Game.Types

-- | Gets current game state and returns portals linkable with given
-- one.
queryLinkablePortals :: Game -> Portal -> [Portal]
queryLinkablePortals game me = do
  let
    allPortals = S.toList $ gamePortals game
    allLinks = S.toList $ gameLinks game
  case L.any (portalUnderField me) $ S.toList $ gameFields game of
    False -> []
    True -> do
      portal <- allPortals
      guard $ notLinked portal
      guard $ noCross portal
      return portal
      where
        notLinked portal =
          let l1NotMember = S.notMember (Link me portal) $ gameLinks game
              l2NotMember = S.notMember (Link portal me) $ gameLinks game
          in l1NotMember && l2NotMember
        noCross portal = L.null $ do
          l <- allLinks
          guard $ linksCross l $ Link me portal
          return l
