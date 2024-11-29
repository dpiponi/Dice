{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Debug.Trace
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Control.Monad.Identity
import Control.Arrow
import Data.Ratio
import Data.Ord
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.List as L

data P p a where
    POrd :: Ord a => M.Map a p -> P p a
    PAny :: [(a, p)] -> P p a

returnP :: (Ord p, Num p, Ord a) => a -> P p a
returnP a = POrd $ M.singleton a 1

-- Trim out zero prob XXX

fromList :: (Num p, Ord a) => [(a, p)] -> M.Map a p
fromList = M.fromListWith (+)

union :: (Num p, Ord a) => M.Map a p -> M.Map a p -> M.Map a p
union = M.unionWith (+)

scaleList :: Num p => p -> [(a, p)] -> [(a, p)]
scaleList weight = map (id *** (weight *))

scaleMap :: (Num p, Ord a) => p -> M.Map a p -> M.Map a p
scaleMap weight = fromList . scaleList weight . M.toList

collectMapM :: (Num p, Monad m) => (a -> m (P p b)) -> [(a, p)] -> m (P p b)
collectMapM f [] = return (PAny [])
collectMapM f ((x, weight) : rest) = do
    fx <- f x
    case fx of
        POrd pdf0 -> do
            let wpdf0 = scaleMap weight pdf0
            frest <- collectMapM f rest
            case frest of
              POrd pdf1 -> return $ POrd $ wpdf0 `union` pdf1
              PAny pdf1 -> return $ POrd $ wpdf0 `union` fromList pdf1
        PAny pdf0 -> do
            let wpdf0 = scaleList weight pdf0
            frest <- collectMapM f rest
            case frest of
              POrd pdf1 -> return $ POrd $ fromList wpdf0 `union` pdf1
              PAny pdf1 -> return $ PAny $ wpdf0 ++ pdf1

runP :: (Num p, Ord a) => P p a -> M.Map a p
runP (POrd pdf) = pdf
runP (PAny pdf) = fromList pdf

runPOrd :: (Num p) => P p a -> [(a, p)]
runPOrd (POrd pdf) = M.toList pdf
runPOrd (PAny pdf) = pdf

-- Slightly unusual among probability monads in that the probabilities
-- themselves need to be sortable.
type Probability p = (Ord p, Fractional p)

data PT p m a = PT { runPT :: m (P p a) }

runPOrdT :: (Num p, Monad m) => PT p m a -> m [(a, p)]
runPOrdT (PT m) = fmap runPOrd m

instance (Monad m, Ord p, Num p) => Functor (PT p m) where
    fmap = liftM

instance (Monad m, Ord p, Num p) => Applicative (PT p m) where
    --pure = return
    pure a = undefined -- trace "Don't call me" $ PT $ return $ PAny [(a, 1)]
    (<*>) = ap

instance (Monad m, Ord p, Num p) => Monad (PT p m) where
        -- XXX Need to find out why this gets called
    return = pure
    m >>= k = PT $ do
        a <- runPOrdT m
        collectMapM (runPT . k) a

instance Probability p => MonadTrans (PT p) where
    lift m = PT $ do
        x <- m
        return $ PAny [(x, 1)]

liftP :: (Monad m, Probability p, Ord a) => m a -> PT p m a
liftP m = PT $ do
    x <- m
    return $ returnP x

class Monad m => MonadProb p m | m -> p where
  bernoulli :: p -> m Bool
  choose :: Ord a => [a] -> m a
  pt :: Ord a => [(a, p)] -> m a
  returnProb :: Ord a => a -> m a

instance (Monad m, Probability p) => MonadProb p (PT p m) where
  bernoulli p = PT $ return $ POrd $ fromList $ [(False, 1-p), (True, p)]

  choose xs = let p = 1 / fromIntegral (length xs)
              in PT $ return $ POrd $ fromList $ map (flip (,) p) xs

  pt pdf = PT $ return $ POrd $ fromList $ pdf
  returnProb a = PT $ return $ POrd $ fromList $ [(a, 1)]

d :: (MonadProb p m, Probability p) => Int -> m Int
d n = choose [1 .. n]

returnP' :: (Monad m, Ord p, Num p, Ord a) => a -> PT p m a
returnP' a = PT $ return $ returnP a

doMin' :: (Monad m, Ord a, Probability p) => [PT p m a] -> PT p m a
doMin' [a] = a
doMin' (a : as) = do
  b <- doMin' as
  c <- a
  returnP' $ min b c

doMax' :: (Monad m, Ord a, Probability p) => [PT p m a] -> PT p m a
doMax' [a] = a
doMax' (a : as) = do
  b <- doMax' as
  c <- a
  returnP' $ max b c

depth' :: (Monad m, Probability p) => Int -> PT p m Int
depth' 0 = d 6
depth' n = do
  let m = doMin' [depth' (n-1), depth' (n-1)]
  -- XXX sharing saves computing m twice but it gets run twice
  -- in a monad!!!!!!!1
  doMax' [m, m]

takeRollM :: (MonadProb p m, Probability p) => Int -> Int -> m [Int]
takeRollM _ 0 = returnProb []
takeRollM 0 _ = returnProb []
takeRollM t r = do
    y <- takeRollM t (r - 1)
    x <- d 6
    returnProb $ take t (L.insertBy (comparing Down) x y)

cutoff :: (Monad m, Probability p, MonadReader Int m) => PT p m a -> PT p m a
cutoff m = do
  depth <- liftP ask
  if depth <= 0
    then PT $ return $ PAny []
    else local (\depth -> depth - 1) m

mapPT :: Probability p => (m (P p a) -> n (P p b)) -> PT p m a -> PT p n b
mapPT f m = PT $ f $ runPT m

instance (Probability p, MonadReader r m) => MonadReader r (PT p m) where
    local = mapPT . local
    ask = lift ask

explode :: (Probability p, MonadReader Int m) => PT p m Int
explode = do
  x <- d 6
  if x == 6
    then do
      y <- cutoff explode
      returnP' $ x + y
    else returnP' x

dumpPDF :: (Show a, Show p) => M.Map a p -> IO ()
dumpPDF pdf = do
    mapM_ (\(a, p) -> do
      putStrLn $ show a ++ " " ++ show a ++ " " ++ show p 
      ) $ M.toList pdf

dumpPDF' :: (Show a, Probability p, Show p) => M.Map a p -> IO ()
dumpPDF' pdf = do
    mapM_ (\(a, p) -> do
      when (p > 1e-20) $ putStrLn $ show a ++ " " ++ show p 
      ) $ M.toList pdf

pdfToCdf :: Num p => [(a, p)] -> [(a, p)]
pdfToCdf pdf =
    let as = map fst pdf
        ps = map snd pdf
    in zip as (scanl1 (+) ps)

cdfToPdf :: Num p => [(a, p)] -> [(a, p)]
cdfToPdf pdf =
    let as = map fst pdf
        ps = map snd pdf
    in zip as $ zipWith (-) ps (0 : ps)

multiplyCdf [] [] pc qc = []
multiplyCdf ((a, p) : ps) [] pc qc =
        (a, p * qc) : multiplyCdf ps [] p qc
multiplyCdf [] ((b, q) : qs) pc qc =
        (b, pc * q) : multiplyCdf [] qs pc q
multiplyCdf ((a, p) : ps) ((b, q) : qs) pc qc =
    case compare a b of
        LT -> (a, p * qc) : multiplyCdf ps ((b, q) : qs) p qc
        GT -> (b, pc * q) : multiplyCdf ((a, p) : ps) qs pc q
        EQ -> (a, p * q) : multiplyCdf ps qs p q

negateCdf [] = []
negateCdf ((a, p) : as) = (a, 1 - p) : negateCdf as

maxP :: (Probability p, Monad m, Ord a) => PT p m a -> PT p m a -> PT p m a
--maxP :: (Probability p, Ord a, MonadProb p m) => m a -> m a -> m a
maxP m0 m1 = PT $ do
  pdf0 <- runPOrdT m0
  pdf1 <- runPOrdT m1
  let cdf0 = pdfToCdf pdf0
  let cdf1 = pdfToCdf pdf1
  let cdf = multiplyCdf cdf0 cdf1 0 0
  return $ POrd $ fromList $ cdfToPdf cdf
  --cdfToPdf cdf

minP :: (Probability p, Monad m, Ord a) => PT p m a -> PT p m a -> PT p m a
minP m0 m1 = PT $ do
  pdf0 <- runPOrdT m0
  pdf1 <- runPOrdT m1
  let cdf0 = pdfToCdf pdf0
  let cdf1 = pdfToCdf pdf1
  let cdf = multiplyCdf (negateCdf cdf0) (negateCdf cdf1) 1 1
  return $ POrd $ fromList $ cdfToPdf $ negateCdf cdf

simpleMinP :: (Probability p, MonadProb p m, Ord a) => m a -> m a -> m a
simpleMinP m0 m1 = do
  x <- m0
  y <- m1
  returnProb $ min x y

simpleMaxP :: (Probability p, MonadProb p m, Ord a) => m a -> m a -> m a
simpleMaxP m0 m1 = do
  x <- m0
  y <- m1
  returnProb $ max x y

maxof :: (Probability p, MonadProb p m) => Int -> m Int
maxof 1 = d 6
maxof n = do
  a <- maxof (n - 1)
  b <- d 6
  returnProb $ max a b

minof :: (Probability p, MonadProb p m) => Int -> m Int
minof 1 = d 6
minof n = do
  a <- minof (n - 1)
  b <- d 6
  returnProb $ min a b

data GameState = G {
  playerHitPoints :: Int,
  monsterHitPoints :: Int,
  numRolls :: Int,
  numMatch :: Int
} deriving (Show, Ord, Eq)

playerTurn :: (Probability p, MonadProb p m) => GameState -> m GameState
playerTurn state =
  if playerHitPoints state == 0 || monsterHitPoints state == 0
    then returnProb state
    else do
      toHit <- d 20
      if toHit >= 11
        then do
          damage <- d 8
          let newHitPoints = max 0 (monsterHitPoints state - damage)
          returnProb $ state { monsterHitPoints = newHitPoints, numRolls = numRolls state + 1, numMatch = numMatch state + (if damage == 8 then 1 else 0) }
        else returnProb state

monsterTurn :: (Probability p, MonadProb p m) => GameState -> m GameState
monsterTurn state =
  if playerHitPoints state == 0 || monsterHitPoints state == 0
    then returnProb state
    else do
      toHit <- d 20
      if toHit >= 11
        then do
          damage <- d 8
          let newHitPoints = max 0 (playerHitPoints state - damage)
          returnProb $ state { playerHitPoints = newHitPoints}
        else returnProb state

simulation :: (Probability p, MonadProb p m) => Int -> GameState -> m GameState
simulation n state = do
  if n <= 0
    then return state
    else do
      state' <- simulation (n - 1) state
      state'' <- playerTurn state'
      monsterTurn state''

expectation :: (Probability p) => P p p -> p
expectation m =
  let pdf = runPOrd m
  in sum $ zipWith (*) (map fst pdf) (map snd pdf)

conditionalExpectation :: (Probability p) => P p p -> p
conditionalExpectation m =
  let pdf = runPOrd m
      totalProb = sum $ map snd pdf
  in (sum $ zipWith (*) (map fst pdf) (map snd pdf)) / totalProb

main = do
{-
    print $ runP $ takeRoll 4 80
    print $ runP $ runIdentity $ runPT $ takeRollM 4 80
    print $ runP $ runReader (runPT $ takeRollM 4 80) 0
    print $ runP $ evalState (runPT $ takeRollM 4 80) 0
    print $ runP $ runReader (runPT explode) 5
-}
{-
    --dumpPDF $ runP $ runReader (runPT explode) 5
    dumpPDF $ runP $ runIdentity (runPT $ maxof 2)
    putStrLn "---"
    dumpPDF $ runP $ runIdentity (runPT $ maxP (d 6) (d 6))
    putStrLn "---"
    dumpPDF $ runP $ evalState (runPT $ takeRollM 4 80) 0
    putStrLn "---"
    putStrLn "---"
    let p0 = choose [1, 4, 7, 9, 11]
    let p1 = choose [2, 6, 7, 8, 9, 13]
    dumpPDF $ runP $ runIdentity (runPT $ simpleMinP p0 p1)
    putStrLn "---"
    dumpPDF $ runP $ runIdentity (runPT $ minP p0 p1)
    putStrLn "---"
    dumpPDF $ runP $ runIdentity (runPT $ simpleMaxP p0 p1)
    putStrLn "---"
    dumpPDF $ runP $ runIdentity (runPT $ maxP p0 p1)
    putStrLn "---"
-}
    let state = G { playerHitPoints = 10, monsterHitPoints = 10, numRolls = 0, numMatch = 0 }
    let pdf = runIdentity $ runPT $
                                      do
                                        state' <- simulation 100 state
                                        let a = fromIntegral $ numRolls state'
                                        let b = fromIntegral $ numMatch state'
                                        returnProb (b / a)
    print (runPOrd pdf :: [(Float, Float)])
    --dumpPDF' $ runP $ runIdentity (runPT $ simulation 100 state)
