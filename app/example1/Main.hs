{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Reader
import Dice

import qualified Data.Map as M

-- Look at https://anydice.com/articles/dnd4-attacks/

-- https://anydice.com/articles/new-world-of-darkness/
nwod :: (Probability p, MonadReader Int m) => Int -> Int -> PT p m Int
nwod threshold again = do
  r <- d 10
  let successes = if r >= threshold then 1 else 0
  if r >= again
    then cutoff $ do
      s <- nwod threshold again
      certainly (successes + s)
    else
      certainly successes

multiNwod :: (Probability p, MonadReader Int m) => Int -> Int -> Int -> PT p m Int
multiNwod _ _ 0 = certainly 0
multiNwod threshold again n = do
  s <- multiNwod threshold again (n - 1)
  r <- nwod threshold again
  certainly $ min 10 (r + s)

main:: IO ()
main = do
    dumpPDF @_ @Double $ M.fromList $ pdfToCdf $ reverse $ M.toList $ runP $ runReader (runPT $ multiNwod {-success-}8 {-again-}9 7) 10
