{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Identity
import Dice

main :: IO ()
main = do
  let dist = runIdentity $ runPT $
                                          do
                                            x <- 49 `nd` 6 >>= ndFrom 100 4
                                            y <- d 6
                                            condition $ x + y == 500
                                            certainly y
  let pdf = normalize dist
  mapM_ print $ runPOrd $ pdf
  print $ expectation pdf
