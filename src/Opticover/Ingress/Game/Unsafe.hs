-- | Stupid manipulation with game types without checks

module Opticover.Ingress.Game.Unsafe
  ( establishLink
  ) where

import Opticover.Import
import Opticover.Ingress.Game.Types


-- | Establish link to portal and create links and fields if
-- possible. Not checks linkability.
establishLink :: Game -> Portal -> Portal -> Game
establishLink = error "Not implemented: establishLink"

liftField :: Game -> Link -> (Maybe Field, Maybe Field)
liftField = error "Not implemented: liftField"
