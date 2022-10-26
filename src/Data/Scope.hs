{-# LANGUAGE QualifiedDo #-}

module Data.Scope
  ( module Data.Scope
  ) where

import qualified Data.IMonad as M
import Data.Indexed
import Prelude hiding (Monad(..))

newtype Scope i o a =
  Scope
    { runScope :: (a -> o) -> i
    }

instance IxFunctor Scope where
  imap f m = ipure f </> m

instance IxApplicative Scope where
  ipure x = Scope $ \k -> k x
  m </> n = M.do
    f <- m
    x <- n
    M.return $ f x

instance M.IxMonad Scope where
  m >>= f = Scope $ \k -> runScope m $ \x -> runScope (f x) k

-- Lifting and lowering
up :: M.IxMonad m => m i j a -> Scope (m i o b) (m j o b) a
up = Scope . (M.>>=)

lift :: M.IxMonad m => a -> Scope (m i j b) (m i j b) a
lift = up . M.return

downWith :: (a -> o) -> Scope i o a -> i
downWith = flip runScope
-- normal lowering is `downwith id`

(<//>) :: (IxApplicative f, IxApplicative g)
  => f i j (g x y (a -> b))
  -> f j k (g y z a)
  -> f i k (g x z b)
m <//> n = ((</>) <<$>> m) </> n
infixr 9 <//>

(<\\>) :: (IxApplicative f, IxApplicative g)
  => f i j (g x y a)
  -> f j k (g y z (a -> b))
  -> f i k (g x z b)
m <\\> n = ((<\>) <<$>> m) </> n
infixr 8 <\\>
