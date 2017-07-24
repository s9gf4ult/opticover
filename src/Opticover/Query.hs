module Opticover.Query where

import Control.Monad
import Data.List as L
import Opticover.Geometry
import Opticover.Types

queryLinkablePortals :: Portal -> [Portal] -> [Link] -> [Portal]
queryLinkablePortals me allPortals allLinks = do
  portal <- allPortals
  guard $ notLinked portal
  guard $ noCross portal
  return portal
  where
    notLinked portal = link me portal `notElem` allLinks
    noCross portal = L.null $ do
      l <- allLinks
      guard $ linksCross l $ link me portal
      return l
