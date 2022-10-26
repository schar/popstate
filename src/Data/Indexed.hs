module Data.Indexed
  ( IxFunctor(..)
  , IxApplicative(..)
  , η
  , (<<$>>)
  ) where

-- Parameterized/Indexed/Hoare classes
-- =======================================

class IxFunctor m where
  -- adjust value without altering effects
  imap :: (a -> b) -> m i j a -> m i j b


class IxFunctor m => IxApplicative m where
  -- create a pure update from a value
  ipure :: a -> m i i a

  -- fully general state-switching application
  infixr 9 </>
  infixr 8 <\>
  (</>) :: m i j (a -> b) -> m j k a -> m i k b

  (<\>) :: m i j a -> m j k (a -> b) -> m i k b
  mx <\> mf = imap (\x k -> k x) mx </> mf

-- Some synonyms
--
η :: IxApplicative m => a -> m i i a
η = ipure

infixl 4 <<$>>
(<<$>>) :: IxFunctor f => (a -> b) -> f i j a -> f i j b
(<<$>>) = imap
