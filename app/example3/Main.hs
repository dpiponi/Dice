{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Identity
import Dice

data GameState = G {
  playerHitPoints :: Int,
  monsterHitPoints :: Int
} deriving (Show, Ord, Eq)

dragonAttack :: (Probability p, MonadProb p m) => m Int
dragonAttack = do
  -- 50/50 chance claws vs. breath
  mode <- d 2
  if mode == 1
    -- 2 claws
    then 2 `nd` 6
    else do
      -- dragon breath
      save <- d 20
      if save < 12
        -- players saves vs dragon breath
        then certainly 0
        else 3 `nd` 6

sword :: (Probability p, MonadProb p m) => m Int
sword = d 8

playerTurn :: (Probability p, MonadProb p m) => GameState -> m GameState
playerTurn gameState = do
  let playerHP = playerHitPoints gameState
  let monsterHP = monsterHitPoints gameState
  if playerHP == 0 || monsterHP == 0
    then certainly gameState
    else do
      toHit <- do
        r <- d 20
        certainly $ r >= 11
      if toHit
        then do
          damage <- sword
          let newHitPoints = max 0 (monsterHP - damage)
          certainly $ gameState { monsterHitPoints = newHitPoints }
        else certainly gameState

monsterTurn :: (Probability p, MonadProb p m) => GameState -> m GameState
monsterTurn gameState = do
  let playerHP = playerHitPoints gameState
  let monsterHP = monsterHitPoints gameState
  if playerHP == 0 || monsterHP == 0
    then certainly gameState
    else do
      toHit <- do
        r <- d 20
        certainly $ r >= 11
      if toHit
        then do
          damage <- dragonAttack
          let newHitPoints = max 0 (playerHP - damage)
          certainly $ gameState { playerHitPoints = newHitPoints}
        else certainly gameState

simulation :: (Probability p, MonadProb p m) => Int -> GameState -> m GameState
simulation n gameState =
  if n <= 0
    then certainly gameState
    else certainly gameState >>= simulation (n - 1) >>= playerTurn >>= monsterTurn

--initialState = certainly (H 3)
--a = updateState initialState

main :: IO ()
main = do
    let gameState = G { playerHitPoints = 10, monsterHitPoints = 100 }
    let pdf = runIdentity $ runPT $ do
                                      s <- simulation 100 gameState
                                      certainly $ playerHitPoints s > 0
    mapM_ print $ runPOrd $ pdf
