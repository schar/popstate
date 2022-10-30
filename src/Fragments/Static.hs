{-# LANGUAGE DeriveFunctor #-}

module Fragments.Static
  ( module Fragments.Static
  ) where

import Data.List.Extra (intersperse)
import Data.PopLens
import Fragments.Lexicon
import Prelude hiding ((<), (>))

newtype Reader g a =
  Reader
    { runReader :: g -> a
    }
  deriving (Functor)

instance Applicative (Reader g) where
  pure = Reader . const
  Reader rf <*> Reader rx = Reader $ \g -> rf g (rx g)

-- variables
z :: (a, u) -> a
z = var l0

sz :: (x, (a, u)) -> a
sz = var l1

ssz :: (x, (y, (a, u))) -> a
ssz = var l2

sssz :: (x, (y, (z, (a, u)))) -> a
sssz = var l3

pro :: (g -> a) -> Reader g a
pro = Reader

pro0 :: Reader (a, u) a
pro0 = pro z -- etc

-- composition
(>) :: (a -> b) -> a -> b
f > x = f x

(<) :: a -> (a -> b) -> b
x < f = f x

(/>) :: Reader g (a -> b) -> Reader g a -> Reader g b
(/>) = (<*>)

(<\) :: Reader g a -> Reader g (a -> b) -> Reader g b
(<\) = flip (/>)

-- called her0
vp1 :: Reader (E, g) (E -> T)
vp1 = pure called /> pro0

-- mary called her0
s1 :: Reader (E, g) T
s1 = pure "mary" <\ vp1

-- abstraction (as currying!)
lam :: Reader (a, g) b -> a -> Reader g b
lam (Reader f) a = Reader $ \g -> f (a, g)

-- john, mary called t0
s2 :: Reader g T
s2 = "john" < lam s1

test :: IO ()
test = do
  putStrLn ""
  sequence_ $ intersperse (putStrLn "")
    [ putStrLn "s1" >> printR s1 ("sue", ())
    , putStrLn "s2" >> printR s2 ()
    ]
  putStrLn ""

printR :: (Show g, Show a) => Reader g a -> g -> IO ()
printR m g = putStrLn $
  "{" ++ show (runReader m g) ++ "; " ++ show g ++ "}"
