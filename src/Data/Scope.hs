module Data.Scope where

import           Data.Indexed

newtype Scope i o a = Scope { runScope :: (a -> o) -> i }

instance IxFunctor Scope where
  imap f m = m >>>= \x -> η $ f x

instance IxApplicative Scope where
  ipure x = Scope $ \k -> k x
  m </> n = m >>>= \f -> n >>>= \x -> η $ f x

instance IxMonad Scope where
  m >>>= f = Scope $ \k -> runScope m (\x -> runScope (f x) k)

-- Lifting and lowering
up :: IxMonad m => m i j a -> Scope (m i o b) (m j o b) a
up = Scope . (>>>=)

lift :: IxMonad m => a -> Scope (m i j b) (m i j b) a
lift = up . ireturn

downWith :: (a -> o) -> Scope i o a -> i
downWith = flip runScope
-- normal lowering is `downwith id`

-- infixr 9 <//>
infixr 8 <\\>
m <//> n = m >>>= \f -> n >>>= \x -> η $ f </> x
m <\\> n = m >>>= \x -> n >>>= \f -> η $ x <\> f
