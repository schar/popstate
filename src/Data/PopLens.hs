{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Data.PopLens
  ( lz
  , lr
  , ll
  , l0
  , l1
  , l2
  , l3
  , Lens
  , get
  , modify -- aka mod in the paper
  , var
  , dvar
  ) where

data IStore a b t = IStore a (b -> t)
  deriving (Functor)

type Lens s t a b = s -> IStore a b t

-- lenses into heterogeneous lists
lz :: Lens a b a b
lz a = IStore a id

lr :: Lens s t a b -> Lens (y, s) (y, t) a b
lr n (y, s) = (y, ) <$> n s

-- lenses into heterogeneous trees
ll :: Lens s t a b -> Lens (s, y) (t, y) a b
ll n (s, y) = (, y) <$> n s

-- some convenient synonyms
l0 :: Lens a b a b
l0 = lz

l1 :: Lens (x, a) (x, b) a b
l1 = lr lz

l2 :: Lens (x, (y, a)) (x, (y, b)) a b
l2 = lr l1

l3 :: Lens (x, (y, (z, a))) (x, (y, (z, b))) a b
l3 = lr l2

-- get and modify
get :: Lens s t a b -> s -> a
get n s = a
  where
    IStore a _ = n s

modify :: Lens s t a b -> (a -> b) -> s -> t
modify n f s = bt (f a)
  where
    IStore a bt = n s

-- static variables
var :: Lens s t (a, u) b -> s -> a
var n = fst . get n

-- dynamic variables with pop
dvar :: Lens s t (a, u) u -> s -> (a, t)
dvar n s = (var n s, modify n snd s)
