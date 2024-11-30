{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Reader
import Dice

-- compare https://anydice.com/articles/legend-of-the-five-rings/

legendOfTheFiveRings :: (Probability p, MonadReader Int m) => PT p m Int
legendOfTheFiveRings = do
  rolls <- rollKeep 3 6 (explode 10)
  certainly $ sum $ rolls

data GameState = G {
  playerHitPoints :: Int,
  monsterHitPoints :: Int,
  target :: Int,
  numRolls :: Int,
  numMatch :: Int
} deriving (Show, Ord, Eq)

playerTurn :: (Probability p, MonadProb p m) => GameState -> m GameState
playerTurn state =
  if playerHitPoints state == 0 || monsterHitPoints state == 0
    then certainly state
    else do
      toHit <- d 20
      if toHit >= 11
        then do
          damage <- d 8
          let newHitPoints = max 0 (monsterHitPoints state - damage)
          certainly $ state { monsterHitPoints = newHitPoints, numRolls = numRolls state + 1, numMatch = numMatch state + (if damage == target state then 1 else 0) }
        else certainly state

monsterTurn :: (Probability p, MonadProb p m) => GameState -> m GameState
monsterTurn state =
  if playerHitPoints state == 0 || monsterHitPoints state == 0
    then certainly state
    else do
      toHit <- d 20
      if toHit >= 11
        then do
          damage <- d 8
          let newHitPoints = max 0 (playerHitPoints state - damage)
          certainly $ state { playerHitPoints = newHitPoints}
        else certainly state

simulation :: (Probability p, MonadProb p m) => Int -> GameState -> m GameState
simulation n state = do
  if n <= 0
    then certainly state
    else do
      state' <- simulation (n - 1) state
      state'' <- playerTurn state'
      monsterTurn state''


main:: IO ()
main = do
    dumpPDF @_ @Double $ runP $ runReader (runPT $ legendOfTheFiveRings) 3
