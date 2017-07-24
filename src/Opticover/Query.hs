module Opticover.Query where

import Control.Monad
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Set as S
import Opticover.Geometry
import Opticover.Ple
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

linksMap :: [Link] -> Map Portal [Link]
linksMap ls = M.fromListWith (++) $ ls >>= toPairs
  where
    toPairs l =
      let (a, b) = unPair $ unLink l
      in [(a, [l]), (b, [l])]

liftFields :: Link -> [Link] -> [Field]
liftFields l allLinks =
  let
    connected a    = not $ S.null $ S.intersection (linkPortals l) (linkPortals a)
    linkCandidates = L.filter connected allLinks
    --
    candidatesMap  = M.difference (linksMap linkCandidates) $ linksMap [l]
    getTuple = \case
      [a, b] -> Just (a, b)
      _      -> Nothing
    pairingLinks = catMaybes $ fmap getTuple $ M.elems candidatesMap

  in (error "FIXME: ")
