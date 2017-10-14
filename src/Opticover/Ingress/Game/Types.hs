module Opticover.Ingress.Game.Types
  ( -- * Portals
    Portal(..)
  , pCoord
  , pName
    -- * Links
  , Link(unLink)
  , link
  , linkPortals
    -- * Fields
  , Field(fieldLinks, fieldPortals)
  , field
    -- * Game
  , Game(gamePortals, gameLinks, gameFields)
  , newGame
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

-- | The link is unordered pair of portals
newtype Link = Link
  { unLink :: Pair Portal
  } deriving (Eq, Ord, Show)

link :: Portal -> Portal -> Link
link a b = Link $ unordPair a b

linkPortals :: Link -> Set Portal
linkPortals l =
  let (a, b) = unPair $ unLink l
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
  { gamePortals :: [Portal]
  , gameLinks   :: [Link]
  , gameFields  :: [Field]
  } deriving (Eq, Ord, Show)

-- | New game has no links nor fields.
newGame :: [Portal] -> Game
newGame portals = Game
  { gamePortals = portals
  , gameLinks   = []
  , gameFields  = []
  }

-- data LinkError
--   = AlreadyLinked Portal Portal
--   | ExceededOutLinks Portal
--   | HaveCross (NonEmpty Link)
--   | UnderField Portal
--   deriving (Eq, Ord, Show)

-- data FieldError

-- data GameError
--   = NoPortal Portal
--   | NotLinkable LinkError
--   | NotFieldable FieldError

-- -- | Creates new link and fields if there. If can not establish link
-- -- then return Left
-- createLink :: Portal -> Portal -> Game -> Either GameError Game
-- createLink pFrom pTo game = do
--   link <- over _Left NotLinkable $ establishLink game pFrom pTo
--   (f1, f2) <- over _Left NotFieldable $ liftField game link
--   return $ game
--     & gameLinks %~ (link:)
--     & gameFields %~ ((catMaybes [f1, f2]) ++)
