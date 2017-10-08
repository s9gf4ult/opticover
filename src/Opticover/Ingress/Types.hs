module Opticover.Ingress.Types
  ( Portal(..)
  , pCoord
  , pName
  , Link(..)
  , link
  , linkPortals
  , Field(fieldLinks, fieldPortals)
  , field
  , LinksMap
  )
where

import Control.Lens
import Control.Monad
import Data.List.NonEmpty as NE
import Data.Map.Strict as M
import Data.Set as S
import Data.Text as T
import Opticover.Geometry
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
-- guaranteed properties
field :: Link -> Link -> Link -> Maybe Field
field a b c = do
  [p1, p2, p3] <- pure $ S.toList $ S.unions $ fmap linkPortals [a, b, c]
  return $ Field
    { fieldLinks = unordTriple a b c
    , fieldPortals = unordTriple p1 p2 p3 }

type LinksMap = Map Portal [Link]

data LinkError
  = AlreadyLinked Portal Portal
  | ExceededOutLinks Portal
  | HaveCross (NonEmpty Link)
  | UnderField Portal
  deriving (Eq, Ord, Show)

data Game = Game
  { _gamePortals :: [Portal]
  , _gameLinks   :: [Link]
  , _gameFields  :: [Field]
  } deriving (Eq, Ord, Show)

data GameError
  = NoPortal Portal
  | NotLinkcable Portal Portal LinkError

newGame :: [Portal] -> Game
newGame portals = Game
  { _gamePortals = portals
  , _gameLinks   = []
  , _gameFields  = []
  }

-- | Creates new link and fields if there
createLink :: Portal -> Portal -> Game -> Either GameError Game
createLink pFrom pTo game = do
  havePortal pFrom
  havePortal pTo

  where
    havePortal portal = do
      unless (elem portal $ gamePortals game) $ throwError $ NoPortal portal
