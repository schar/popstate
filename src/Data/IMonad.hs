module Data.IMonad (IxMonad(..)) where

import Data.Indexed
import Prelude hiding (Monad(..))

class IxApplicative m => IxMonad m where
  -- create a pure update from a value
  return :: a -> m i i a
  return = ipure

  -- scope an update over a continuation
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b
  (>>) :: m i j a -> m j k b -> m i k b
  x >> y = x >>= const y

  ijoin :: m i j (m j k b) -> m i k b
  ijoin m = m >>= id
