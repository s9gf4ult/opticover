module Opticover.Ingress.Optimize where

import Opticover.Import
import Opticover.Ingress.Types


data GameAction = LinkPortals Portal Portal

-- | Return all possible next game steps. Now supports only
-- linking. If empty list is returned then game is over and no next
-- steps can be performed.
possibleNextSteps :: Game -> [(Game, GameAction)]
possibleNextSteps game = do
  startPortal <- gamePortals game
  nextPortal <- queryLinkablePortals game startPortal
  let newGame = establishLink game startPortal nextPortal
  return (newGame, LinkPortals startPortal nextPortal)

-- | Tries 'possibleNextSteps' until it returns empty linst. The head
-- of returned [GameAction] is the last action applied to game.
allPossibleStrategies :: Game -> [(Game, [GameAction])]
allPossibleStrategies = go []
  where
    go :: [GameAction] -> Game -> [(Game, [GameAction])]
    go acc game = do
      let nextSteps = possibleNextSteps game
      case nextSteps of
        [] -> return (game, acc)
        _ -> do
          (newGame, step) <- nextSteps
          go (step : acc) newGame
