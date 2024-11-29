{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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

{-
instance (Num p) => Functor (P p) where
    fmap = liftM

instance (Num p) => Applicative (P p) where
    pure = return
    (<*>) = ap
-}

{-
instance (Num p) => Monad (P p) where
    return x = undefined -- PAny [(x, 1)]
    m >>= f =
        let pdf = unP m
        in collectMap f pdf
-}

{-
instance (Ord p, Num p) => Foldable (P p) where
    foldMap f (POrd pdf) = foldMap f (map fst $ M.toList pdf)
    foldMap f (PAny pdf) = foldMap f (map fst pdf)

instance (Ord p, Num p) => Traversable (P p) where
    mapM f (POrd pdf) = foldMap f (map fst $ M.toList pdf)
-}

returnP :: (Ord p, Num p, Ord a) => a -> P p a
returnP a = POrd $ M.singleton a 1

-- Trim out zero prob XXX

{-
unP :: P p a -> [(a, p)]
unP (POrd pdf) = M.toList pdf
unP (PAny pdf) = pdf
-}

fromList :: (Num p, Ord a) => [(a, p)] -> M.Map a p
fromList = M.fromListWith (+)

union :: (Num p, Ord a) => M.Map a p -> M.Map a p -> M.Map a p
union = M.unionWith (+)

scaleList :: Num p => p -> [(a, p)] -> [(a, p)]
scaleList weight = map (id *** (weight *))

scaleMap :: (Num p, Ord a) => p -> M.Map a p -> M.Map a p
scaleMap weight = fromList . scaleList weight . M.toList

{-
collectMap :: Num p => (b -> P p a) -> [(b, p)] -> P p a
collectMap f []  = PAny []
collectMap f ((x, weight) : rest) =
    case f x of
        POrd pdf0 ->
            let wpdf0 = scaleMap weight pdf0
            in case collectMap f rest of
              POrd pdf1 -> POrd $ wpdf0 `union` pdf1
              PAny pdf1 -> POrd $ wpdf0 `union` fromList pdf1
        PAny pdf0 ->
            let wpdf0 = scaleList weight pdf0
            in case collectMap f rest of
              POrd pdf1 -> POrd $ fromList wpdf0 `union` pdf1
              PAny pdf1 -> PAny $ wpdf0 ++ pdf1
-}

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

{-
instance (Num p, Applicative (P p)) => Alternative (P p) where
    empty = PAny []
    a <|> b = collectMap (\x -> if x then a else b) [(False, 1), (True, 1)]

instance (Num p, Monad (P p), Applicative (P p)) => MonadPlus (P p)
-}

runP :: (Num p, Ord a) => P p a -> M.Map a p
runP (POrd pdf) = pdf
runP (PAny pdf) = fromList pdf

{-
runP'' :: (Num p) => P p a -> [(a, p)]
runP'' (POrd pdf) = M.toList pdf
runP'' (PAny pdf) = pdf
-}

{-
chooseP :: (Probability p, Ord a) =>
           [a] -> P p a
chooseP xs = let p = 1/fromIntegral (length xs)
             in POrd $ fromList $ map (flip (,) p) xs
-}

{-
bernoulliP :: (Fractional p, Ord p) =>
              p -> P p Bool
bernoulliP p = POrd $ fromList $ [(False, 1-p), (True, p)]
-}

-- Slightly unusual among probability monads in that the probabilities
-- themselves need to be sortable.
type Probability p = (Ord p, Fractional p)

{-
d :: Probability p => Int -> P p Int
d n = chooseP [1 .. n]
-}

{-
takeRoll :: Probability p => Int -> Int -> P p [Int]
takeRoll _ 0 = returnP []
takeRoll 0 _ = returnP []
takeRoll t r = do
    y <- takeRoll t (r - 1)
    x <- d 6
    returnP $ take t (L.insertBy (comparing Down) x y)
-}

{-
doMin :: (Ord a, Probability p) => [P p a] -> P p a
doMin [a] = a
doMin (a : as) = do
  b <- doMin as
  c <- a
  returnP $ min b c

doMax :: (Ord a, Probability p) => [P p a] -> P p a
doMax [a] = a
doMax (a : as) = do
  b <- doMax as
  c <- a
  returnP $ max b c

takeRoll :: (Probability p, Monad m) => Int -> Int -> PT p m [Int]
takeRoll _ 0 = returnP' []
takeRoll 0 _ = returnP' []
takeRoll t r = do
    y <- takeRoll t (r - 1)
    x <- d 6
    returnP' $ take t (L.insertBy (comparing Down) x y)

runP' :: (Ord a) => P Rational a -> M.Map a Rational
runP' = runP
-}

{-
depth 0 = d 6
depth n = do
  let m = doMin [depth (n-1), depth (n-1)]
  doMax [m, m]
-}

data PT p m a = PT { runPT :: m (P p a) }

runP''T :: (Num p, Monad m) => PT p m a -> m [(a, p)]
runP''T (PT m) = do
    p <- m
    return (runP'' p)

instance (Monad m, Ord p, Num p) => Functor (PT p m) where
    fmap = liftM

instance (Monad m, Ord p, Num p) => Applicative (PT p m) where
    pure = return
    (<*>) = ap

instance (Monad m, Ord p, Num p) => Monad (PT p m) where
        -- XXX Need to find out why this gets called
    return a = PT $ return $ PAny [(a, 1)]
    m >>= k = PT $ do
        a <- runP''T m
        collectMapM (runPT . k) a

instance Probability p => MonadTrans (PT p) where
    lift m = PT $ do
        x <- m
        return $ PAny [(x, 1)]

liftP :: (Monad m, Probability p, Ord a) => m a -> PT p m a
liftP m = PT $ do
    x <- m
    return $ returnP x

bernoulli :: (Fractional p, Monad m, Ord p) => p -> PT p m Bool
bernoulli p = PT $ return $ POrd $ fromList $ [(False, 1-p), (True, p)]

choose :: (Probability p, Monad m, Ord a) => [a] -> PT p m a
choose xs = let p = 1 / fromIntegral (length xs)
            in PT $ return $ POrd $ fromList $ map (flip (,) p) xs

d :: (Monad m, Probability p) => Int -> PT p m Int
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

{-
depth'' = do
    n <- liftP ask
    depth' n
-}

{-
test2 = do
    p <- liftP ask
    x <- bernoulliP' p
    y <- bernoulliP' p
    return (x, y)
-}

takeRollM :: (Monad m, Probability p) => Int -> Int -> PT p m [Int]
takeRollM _ 0 = returnP' []
takeRollM 0 _ = returnP' []
takeRollM t r = do
    y <- takeRollM t (r - 1)
    x <- d 6
    returnP' $ take t (L.insertBy (comparing Down) x y)

{-
simple :: P Rational Int
simple = do
  x <- d 6
  y <- d 6
  returnP (x + y)
-}

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
maxP m0 m1 = PT $ do
  pdf0 <- runP''T m0
  pdf1 <- runP''T m1
  let cdf0 = pdfToCdf pdf0
  let cdf1 = pdfToCdf pdf1
  let cdf = multiplyCdf cdf0 cdf1 0 0
  return $ POrd $ fromList $ cdfToPdf cdf

minP :: (Probability p, Monad m, Ord a) => PT p m a -> PT p m a -> PT p m a
minP m0 m1 = PT $ do
  pdf0 <- runP''T m0
  pdf1 <- runP''T m1
  let cdf0 = pdfToCdf pdf0
  let cdf1 = pdfToCdf pdf1
  let cdf = multiplyCdf (negateCdf cdf0) (negateCdf cdf1) 1 1
  return $ POrd $ fromList $ cdfToPdf $ negateCdf cdf

simpleMinP :: (Probability p, Monad m, Ord a) => PT p m a -> PT p m a -> PT p m a
simpleMinP m0 m1 = do
  x <- m0
  y <- m1
  returnP' $ min x y

simpleMaxP :: (Probability p, Monad m, Ord a) => PT p m a -> PT p m a -> PT p m a
simpleMaxP m0 m1 = do
  x <- m0
  y <- m1
  returnP' $ max x y

maxof :: (Probability p, Monad m) => Int -> PT p m Int
maxof 1 = d 6
maxof n = do
  a <- maxof (n - 1)
  b <- d 6
  returnP' $ max a b

minof :: (Probability p, Monad m) => Int -> PT p m Int
minof 1 = d 6
minof n = do
  a <- minof (n - 1)
  b <- d 6
  returnP' $ min a b
  
main = do
{-
    print $ runP $ takeRoll 4 80
    print $ runP $ runIdentity $ runPT $ takeRollM 4 80
    print $ runP $ runReader (runPT $ takeRollM 4 80) 0
    print $ runP $ evalState (runPT $ takeRollM 4 80) 0
    print $ runP $ runReader (runPT explode) 5
-}
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
