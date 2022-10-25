{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Data.PopLens
  ( lr
  , ll
  , l0
  , l1
  , l2
  , l3
  , Lens
  , get
  , modify
  , pro
  , pro''
  ) where

data IStore a b t = IStore a (b -> t)
  deriving (Functor)

type Lens s t a b = s -> IStore a b t

-- lenses into heterogeneous lists
lz :: Lens a b a b
lz a = IStore a id

lr :: Lens s t a b -> Lens (y, s) (y, t) a b
lr n (y, s) = (y, ) <$> n s

-- get and modify
get :: Lens s t a b -> s -> a
get n s = a
  where
    IStore a ct = n s

modify :: Lens s t a b -> (a -> b) -> s -> t
modify n f s = bt (f a)
  where
    IStore a bt = n s

-- static variables
type Reader i a = i -> a

pro :: Lens s t (a, u) b -> Reader s a
pro n s = fst $ get n s
-- pro (lr (lr lz)) :: Reader (y1, (y2, (a, u))) a

-- dynamic variables
type State i o a = i -> (a, o)

pro' :: Lens s t (a, u) b -> State s s a
pro' n s = (pro n s, s)
-- pro' (lr (lr lz)) :: State (y1, (y2, (a, u))) (y1, (y2, (a, u))) a

-- lenses into heterogeneous trees
ll :: Lens s t a b -> Lens (s, y) (t, y) a b
ll n (s, y) = (, y) <$> n s
-- pro' (ll (lr lz)) :: State ((y1, (a, u)), y2) ((y1, (a, u)), y2) a

-- with pop
pro'' :: Lens s t (a, u) u -> State s t a
pro'' n s = modify n snd <$> pro' n s
-- pro'' (ll (lr lz)) :: State ((y1, (a, u)), y2) ((y1, u), y2) a

l0 = lz
l1 = lr lz
l2 = lr l1
l3 = lr l2

rp0 = pro lz
rp1 = pro l1
rp2 = pro l2
rp3 = pro l3

sp0 = pro'' lz
sp1 = pro'' l1
sp2 = pro'' l2
sp3 = pro'' l3



