{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TupleSections #-}

module Data.State
  ( module Data.State
  ) where

import qualified Data.IMonad as M
import Data.Indexed
import Data.PopLens

-- Parameterized State
-- =======================================

class M.IxMonad m => IxMonadState m where
  iget :: m i i i
  iput :: j -> m i j ()

imodify :: IxMonadState m => (i -> j) -> m i j ()
imodify f = M.do
  x <- iget
  iput $ f x

-- igets :: IxMonadState m => (i -> a) -> m i i a
-- igets f = M.do
--   x <- iget
--   M.return $ f x

ipush :: IxMonadState m => a -> m i (a, i) a
ipush x = M.do
  imodify (x, )
  M.return x

mpush :: IxMonadState m => m i j a -> m i (a, j) a
mpush m = M.do
  x <- m
  ipush x

lgets :: IxMonadState m => Lens s t (a, b) b -> m s t a
lgets l = M.do
  i <- iget
  let (x, j) = pro'' l i
  iput j
  M.return x

newtype State i j a =
  State
    { runState :: i -> [(a, j)]
    }

instance IxFunctor State where
  imap f m = ipure f </> m

instance IxApplicative State where
  ipure x = State $ \i -> [(x, i)]
  mf </> mx = M.do
    f <- mf
    x <- mx
    M.return $ f x

instance M.IxMonad State where
  State m >>= f =
    State $ \i -> [(b, k) | (a, j) <- m i, (b, k) <- runState (f a) j]

instance IxMonadState State where
  iget   = State $ \i -> [(i , i)]
  iput j = State $ const [((), j)]

evalState :: State i j a -> i -> [a]
evalState m i = map fst (runState m i)

ilift :: [a] -> State i i a
ilift xs = State $ \i -> [(x, i) | x <- xs]

iplus :: State i j a -> State i j a -> State i j a
m `iplus` n = State $ \i -> runState m i ++ runState n i

isum :: [State i j a] -> State i j a
isum = foldr iplus (State $ const [])

v0 :: State (a, i) i a
v0 = lgets l0

v1 :: IxMonadState m => m (x, (a, b)) (x, b) a
v1 = lgets l1

v2 :: IxMonadState m => m (x, (y, (a, b))) (x, (y, b)) a
v2 = lgets l2

v3 :: IxMonadState m => m (x, (y, (z, (a, b)))) (x, (y, (z, b))) a
v3 = lgets l3
