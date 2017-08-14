module Opticover.Types where

import Control.Lens
import Data.Map.Strict as M
import Data.Set as S
import Data.Text as T
import Opticover.Ple

data Point = Point
  { _pX :: Double
  , _pY :: Double
  } deriving (Eq, Ord, Show)

makeLenses ''Point

newtype Vec = Vec
  { unVec :: Point
  } deriving (Eq, Ord, Show)

makePrisms ''Vec

data Line = Line
  { _lBase      :: !Point
  , _lDirection :: !Vec
  } deriving (Eq, Ord, Show)

makeLenses ''Line

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
newtype Field = Field
  { unField :: Triple Link
  } deriving (Eq, Ord, Show)

field :: Link -> Link -> Link -> Field
field a b c = Field $ unordTriple a b c

fieldLinks :: Field -> [Link]
fieldLinks f =
  let (a, b, c) = unTriple $ unField f
  in [a, b, c]

fieldPortals :: Field -> Set Portal
fieldPortals f = S.fromList $ fieldLinks f >>= (S.toList . linkPortals)

type LinksMap = Map Portal [Link]
