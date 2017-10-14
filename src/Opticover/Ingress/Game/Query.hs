module Opticover.Ingress.Game.Query where

import Data.List as L
import Opticover.Import
import Opticover.Ingress.Game.Calc
import Opticover.Ingress.Game.Types

-- | Gets current game state and returns portals linkable with given
-- one.
queryLinkablePortals :: Game -> Portal -> [Portal]
queryLinkablePortals game me = do
  case L.any (portalUnderField me) (gameFields game) of
    False -> []
    True -> do
      portal <- gamePortals game
      guard $ notLinked portal
      guard $ noCross portal
      return portal
      where
        notLinked portal = link me portal `notElem` (gameLinks game)
        noCross portal = L.null $ do
          l <- gameLinks game
          guard $ linksCross l $ link me portal
          return l
