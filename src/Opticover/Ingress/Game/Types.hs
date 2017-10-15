module Opticover.Ingress.Game.Types
  ( -- * Portals
    Portal(..)
  , pCoord
  , pName
    -- * Links
  , Link(..)
  , linkPortalFrom
  , linkPortalTo
  , linkPortals
    -- * Fields
  , Field(fieldLinks, fieldPortals)
  , field
    -- * Game
  , Game(gamePortals, gameLinks, gameFields)
  , newGame
  , addGameLink
  , addGameFields
  )
where

import Data.List.NonEmpty as NE
import Data.Map.Strict as M
import Data.Set as S
import Data.Text as T
import Opticover.Geometry
import Opticover.Import
import Opticover.Ple

data Portal = Portal
  { _pCoord :: !Point
  , _pName  :: !Text
  } deriving (Eq, Ord, Show)

makeLenses ''Portal

-- | The link is an ordered pair of porals. First portal is portal the
-- link was established from, and second is portal the link
-- established to.
data Link = Link
  { _linkPortalFrom :: !Portal
  , _linkPortalTo   :: !Portal
  } deriving (Eq, Ord, Show)

makeLenses ''Link

linkPortals :: Link -> Set Portal
linkPortals l =
  let Link a b = l
  in S.fromList [a, b]

-- | The field is unordered triple of links
data Field = Field
  { fieldLinks   :: !(Triple Link)
  , fieldPortals :: !(Triple Portal)
  } deriving (Eq, Ord, Show)

-- | Smart constructor for field, the only way to create field with
-- guaranteed properties. Returns Nothing if three links have more or
-- less than three common portals
field :: Link -> Link -> Link -> Maybe Field
field a b c = do
  [p1, p2, p3] <- pure $ S.toList $ S.unions $ fmap linkPortals [a, b, c]
  return $ Field
    { fieldLinks = unordTriple a b c
    , fieldPortals = unordTriple p1 p2 p3 }

data Game = Game
  { gamePortals :: !(Set Portal)
  , gameLinks   :: !(Set Link)
  , gameFields  :: !(Set Field)
  } deriving (Eq, Ord, Show)

-- | New game has no links nor fields.
newGame :: Set Portal -> Game
newGame portals = Game
  { gamePortals = portals
  , gameLinks   = S.empty
  , gameFields  = S.empty
  }

addGameLink :: Set Link -> Game -> Game
addGameLink links game = game
  { gameLinks = S.union links $ gameLinks game }

addGameFields :: Set Field -> Game -> Game
addGameFields fields game = game
  { gameFields = S.union fields $ gameFields game }
