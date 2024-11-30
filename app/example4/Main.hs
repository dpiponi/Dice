{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Identity
import Dice

data GameState = G {
  playerHitPoints :: Int,
  monsterHitPoints :: Int,
  target :: Int,
  numRolls :: Int,
  numMatch :: Int
} deriving (Show, Ord, Eq)

dragonAttack :: (Probability p, MonadProb p m) => m Int
dragonAttack = do
  -- 50/50 chance claws vs. breath
  mode <- d 2
  if mode == 1
    -- claws
    then 2 `nd` 6
    else do
      -- dragon breath
      save <- d 20
      -- players saves vs dragon breath
      if save < 12 then certainly 0 else 3 `nd` 6

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
          certainly $ gameState {
            monsterHitPoints = newHitPoints,
            numRolls = numRolls gameState + 1,
            numMatch = numMatch gameState + (if damage == target gameState then 1 else 0)
            }
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
simulation n gameState = do
  if n <= 0
    then certainly gameState
    else certainly gameState >>= simulation (n - 1) >>= playerTurn >>= monsterTurn

--initialState = certainly (H 3)
--a = updateState initialState

main :: IO ()
main = do
    forM_ [1..8] $ \dieRoll -> do
      let gameState = G { playerHitPoints = 10, monsterHitPoints = 30, target = dieRoll, numRolls = 0, numMatch = 0 }
      let pdf = runIdentity $ runPT $
                                        do
                                          gameState' <- simulation 30 gameState
                                          condition $ monsterHitPoints gameState' == 0
                                          --condition $ numRolls gameState' > 0
                                          let a = fromIntegral $ numRolls gameState'
                                          let b = fromIntegral $ numMatch gameState'
                                          --certainly b
                                          certainly (b, a)
                                          --certainly (b / a)
      let a = conditionalExpectation $ fmap fst pdf :: Double
      let b = conditionalExpectation $ fmap snd pdf 
      putStrLn $ show dieRoll ++ " " ++ show dieRoll ++ " " ++ show (a / b)

