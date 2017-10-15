-- | Stupid manipulation with game types without checks

module Opticover.Ingress.Game.Unsafe
  ( establishLink
  ) where

import Data.Map.Strict as M
import Data.Set as S
import Opticover.Import
import Opticover.Ingress.Game.Types

type LinksMap = Map Portal [Link]

-- | Establish link to portal and create links and fields if
-- possible. Not checks linkability.
establishLink :: Game -> Portal -> Portal -> Game
establishLink game pFrom pTo =
  let
    l = Link pFrom pTo
    lm = linksMap $ S.toList $ gameLinks game
    (f1, f2) = linkFields l lm
    fields = catMaybes [f1, f2]
    newGame = addGameLinks [l] $ addGameFields fields game
  in newGame

liftFields
  :: Link
  -- ^ Newly created link
  -> LinksMap
  -- ^ List of existed links
  -> (Maybe Field, Maybe Field)
liftFields l allLinks =
  let
    Link a b = l
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

linksMap :: [Link] -> LinksMap
linksMap ls = M.fromListWith (++) $ ls >>= toPairs
  where
    toPairs l =
      let (a, b) = unPair $ unLink l
      in [(a, [l]), (b, [l])]
