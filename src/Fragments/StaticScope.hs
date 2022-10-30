module Fragments.StaticScope
  ( module Fragments.StaticScope
  ) where

import Data.List.Extra (intersperse)
import qualified Data.IMonad as M
import Data.Indexed
import Data.Scope
import Fragments.Lexicon
import Fragments.Static
import Prelude hiding ((<), (>), (*))

-- basic combinators
k1 :: (a -> b) -> Scope i j a -> Scope i j b
k1 = (<<$>>)

k2 :: (a -> b -> c) -> Scope i j a -> Scope j k b -> Scope i k c
k2 f l r = (f <<$>> l) </> r

u :: a -> Scope i i a
u = M.return

-- Shan & Barker's Down
dPrelim :: Scope i a a -> i
dPrelim = downWith id

-- everyone's mom called her0
everyone :: Scope (Reader g T) (Reader (E, g) T) E
everyone =
  Scope $ \k -> Reader $ \g -> "forall x." ++ runReader (k "x") ("x", g)

sent1 :: Reader g T
sent1 = dPrelim tower1

tower1 :: Scope (Reader g T) (Reader (E, g) T) (Reader (E, g) T)
tower1 = subj `op` vp
  where
    op = k2 (<\)
    subj :: Scope (Reader g T) (Reader (E, g) T) (Reader h E)
    subj = op1 (everyone `op2` u mom)
      where
        op1 = k1 pure
        op2 = k2 (<)
    vp :: Scope r r (Reader (E, g) (E -> T))
    vp = u (pure called /> pro0)

-- her0 mom called everyone (crossover)
sent2 :: Reader g T
sent2 = dPrelim tower2

tower2 :: Scope (Reader g T) (Reader (E, g) T) (Reader (E, g) T)
tower2 = subj `op` vp
  where
    op = k2 (<\)
    subj :: Scope i i (Reader (E, g) E)
    subj = u (pro0 <\ pure mom)
    vp :: Scope (Reader g T) (Reader (E, g) T) (Reader (E, g) (E -> T))
    vp = op1 (u called `op2` everyone)
      where
        op1 = k1 pure
        op2 = k2 (>)

-- better lowering
d :: Scope i (Reader g a) (Reader () a) -> i
d = downWith (\m -> Reader $ const $ runReader m ())

-- prevents both prior derivations from concluding:
-- bad1 = d tower1
-- bad2 = d tower2
-- Error: Couldn't match type ‘(E, g)’ with ‘()’

-- monadic lifting into Scope
instance Monad (Reader g) where
  return = pure
  Reader e >>= k = Reader $ \g -> runReader (k $ e g) g

(*) :: Reader g a -> Scope (Reader g b) (Reader g b) a
(*) = Scope . (>>=)

-- everyone's mom called her0, consistent with d
sent3 :: Reader g T
sent3 = d tower3

tower3 :: Scope (Reader g T) (Reader (E, g) T) (Reader h T)
tower3 = op1 $ subj `op` vp
  where op = k2 (<)
        op1 = k1 pure
        -- subj :: Scope (Reader g T) (Reader (E, g) T) E
        subj = everyone `op` u mom
        -- vp :: Scope  (Reader (E, g) T) (Reader (E, g) T) (E -> T)
        vp = u called `op2` (*) pro0
          where
            op2 = k2 (>)

-- her0 mom called everyone -- surface scope, no binding
sent4 :: Reader (E, g) T
sent4 = d tower4

tower4 :: Scope (Reader (E, g) T) (Reader (E, (E, g)) T) (Reader h T)
tower4 = op1 $ subj `op` vp
  where op = k2 (<)
        op1 = k1 pure
        subj :: Scope (Reader (E, g) T) (Reader (E, g) T) E
        subj = (*) pro0 `op` u mom
        vp :: Scope  (Reader g T) (Reader (E, g) T) (E -> T)
        vp = u called `op2` everyone
          where
            op2 = k2 (>)

test :: IO ()
test = do
  putStrLn ""
  sequence_ $ intersperse (putStrLn "")
    [ putStrLn "sent1" >> printR sent1 ()
    , putStrLn "sent2" >> printR sent2 ()
    , putStrLn "sent3" >> printR sent3 ()
    , putStrLn "sent4" >> printR sent4 ("mary", ())
    ]
  putStrLn ""
