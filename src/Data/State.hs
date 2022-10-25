{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.State
  ( module Data.State ) where

import Data.Indexed
import Data.PopLens

-- Parameterized State
-- =======================================

class IxMonad m => IxMonadState m where
  iget :: m i i i
  iput :: j -> m i j ()

imodify :: IxMonadState m => (i -> j) -> m i j ()
imodify f = iget ★ \x -> iput $ f x

igets :: IxMonadState m => (i -> a) -> m i i a
igets f = iget ★ \x -> ireturn $ f x

ipush :: IxMonadState m => a -> m i (a, i) a
ipush x = imodify (\i -> (x, i)) >>> ireturn x

lgets :: IxMonadState m => Lens s t (a, b) b -> m s t a
lgets l =
  iget >>>= \i ->
    let (x, j) = pro'' l i
     in iput j >>> ireturn x

newtype State s0 s1 a =
  State
    { runState :: s0 -> [(a, s1)]
    }

instance IxFunctor State where
  imap f (State m) = State $ \s0 -> [(f x, s1) | (x, s1) <- m s0]

instance IxApplicative State where
  ipure x = State $ \s0 -> [(x, s0)]
  (State mf) </> (State mx) =
    State $ \s0 -> [(f x, s2) | (f, s1) <- mf s0, (x, s2) <- mx s1]

instance IxMonad State where
  ireturn x = State $ \s0 -> [(x, s0)]
  (State m) >>>= k =
    State $ \s0 -> [(b, s2) | (a, s1) <- m s0, (b, s2) <- runState (k a) s1]

instance IxMonadState State where
  iget = State $ \i -> [(i, i)]
  iput j = State $ \_ -> [((), j)]

evalState :: State s0 s1 a -> s0 -> [a]
evalState m s = map fst (runState m s)

ilift :: [a] -> State s0 s0 a
ilift xs = State $ \s -> [(x, s) | x <- xs]

iplus :: State s0 s1 a -> State s0 s1 a -> State s0 s1 a
m `iplus` n = State $ \s -> runState m s ++ runState n s

isum :: [State s0 s1 a] -> State s0 s1 a
isum = foldr iplus (State $ const [])

v0 :: State (a, i) i a
v0 = lgets l0

v1 :: IxMonadState m => m (x, (a, b)) (x, b) a
v1 = lgets l1

v2 :: IxMonadState m => m (x, (y, (a, b))) (x, (y, b)) a
v2 = lgets l2

v3 :: IxMonadState m => m (x, (y, (z, (a, b)))) (x, (y, (z, b))) a
v3 = lgets l3

