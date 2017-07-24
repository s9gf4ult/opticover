module Opticover.Types where

import Control.Lens
import Data.Set as S
import Data.Text as T
import Opticover.Ple

data Point = Point
  { _pX :: Double
  , _pY :: Double
  } deriving (Eq, Ord)

makeLenses ''Point

data Portal = Portal
  { _pCoord :: Point
  , _pName :: Text
  } deriving (Eq, Ord)

makeLenses ''Portal

-- | The link is unordered pair of portals
newtype Link = Link
  { unLink :: Pair Portal
  } deriving (Eq, Ord)

link :: Portal -> Portal -> Link
link a b = Link $ unordPair a b

linkPortals :: Link -> Set Portal
linkPortals l =
  let (a, b) = unPair $ unLink l
  in S.fromList [a, b]

-- | The field is unordered triple of links
newtype Field = Field
  { unField :: Triple Link
  } deriving (Eq, Ord)

field :: Link -> Link -> Link -> Field
field a b c = Field $ unordTriple a b c
