module Opticover.Ingress.Query where

import Control.Lens
import Control.Monad
import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.Set as S
import Opticover.Geometry
import Opticover.Ingress.Types
import Opticover.Ple

type LinksMap = Map Portal [Link]

queryLinkablePortals
  :: Portal
  -- ^ Portal we are trying to link from
  -> [Portal]
  -- ^ List of all portals
  -> [Link]
  -- ^ List of all links
  -> [Field]
  -- ^ List of all fields
  -> [Portal]
queryLinkablePortals me allPortals allLinks allFields =
  if L.any (portalUnderField me) allFields then [] else linkable
  where
    linkable = do
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

linksMap :: [Link] -> LinksMap
linksMap ls = M.fromListWith (++) $ ls >>= toPairs
  where
    toPairs l =
      let (a, b) = unPair $ unLink l
      in [(a, [l]), (b, [l])]

leftRightSplit :: Link -> [Portal] -> ([Portal], [Portal])
leftRightSplit l portals = L.partition leftPortal portals
  where
    leftPortal p =
      let (a, b) = unPair $ unLink l
          vec1 = portalVec a b
          vec2 = portalVec a p
      in vecPerpProduct vec1 vec2 > 0

fieldSquare :: Field -> Double
fieldSquare field =
  let
    (a, b, c) = unTriple $ fieldPortals field
    vec1 = portalVec a b
    vec2 = portalVec a c
  in vecSquare vec1 vec2

liftFields
  :: Link
  -- ^ Newly created link
  -> LinksMap
  -- ^ List of existed links
  -> [Field]
liftFields l allLinks =
  let
    (a, b) = unPair $ unLink l
    aLinks = linksMap $ join $ F.toList $ M.lookup a allLinks
    bLinks = linksMap $ join $ F.toList $ M.lookup b allLinks
    connectedLinks = M.intersectionWith (++) aLinks bLinks
    -- ^ map of links forming triangle with link l. Keys are third
    -- portals
    (lefts, rights) = leftRightSplit l $ M.keys connectedLinks
    -- ^ Two list of points: left from link and right from link
    maxSquare a b = compare (fieldSquare b) (fieldSquare a)
    toField :: Portal -> Field
    toField p = fromMaybe (error "Strange shit") $ do
      [a, b] <- M.lookup p connectedLinks
      field l a b
    getMaxField portals = F.toList $ listToMaybe $ sortBy maxSquare
      $ fmap toField portals
  in getMaxField lefts ++ getMaxField rights
