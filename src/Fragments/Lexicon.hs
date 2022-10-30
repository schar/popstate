module Fragments.Lexicon
  ( module Fragments.Lexicon
  ) where

import Data.List.Extra (groupSort, intercalate)
import Data.Scope
import Data.State
import Prelude hiding (all, any)

type E = String
type T = String

see :: E -> E -> T
see x y = y ++ " saw " ++ x

called :: E -> E -> T
called x y = y ++ " called " ++ x

pet :: E -> E -> T
pet x y = y ++ " pet " ++ x

dog :: E -> E
dog x = x ++ "'s dog"

mom :: E -> E
mom x = x ++ "'s mom"

sell :: E -> E -> E -> T
sell x y z = z ++ " sold " ++ x ++ " to " ++ y

buy :: E -> E -> E -> T
buy x y z = z ++ " bought " ++ x ++ " from " ++ y

own', see' :: [(E, E)]
own' = [("john", "jd1"), ("mary", "md1"), ("mary", "md2")]
see' = [("john", "djm"), ("mary", "djm"), ("mary", "dm")]

-- static conjunction
conj :: T -> T -> T
conj y x = all [x, y]

-- dynamic disjunction
disj :: [a] -> State i i a
disj = ilift
-- obviates `iplus`? either way correctly requires synchronous dref
-- introduction in dynamic disjunction

-- mock-Boolean operators for convenience
trueOn :: (j -> Bool) -> State i j T -> i -> T
trueOn postsup m i = any [p | (p, j) <- runState m i, postsup j]

true :: State i j T -> i -> T
true = trueOn (const True)

all :: [T] -> T
all xs =
  if null xs
    then "T"
    else intercalate " and " xs

any :: [T] -> T
any xs =
  if null xs
    then "F"
    else "(" ++ intercalate " or " xs ++ ")"

dyn ::
     ([T] -> T)
  -> ([T] -> T)
  -> State i j E
  -> Scope (State i i T) (State j k T) E
dyn det str p = Scope $ \c -> State $ \h -> [(det $ map (scope c) (restr h), h)]
  where
    restr h = groupSort (runState p h)
    scope c (x, bss) = str [true (c x) bs | bs <- bss]

everyS, everyW :: State i j E -> Scope (State i i T) (State j k T) E
everyS = dyn all all
everyW = dyn all any

