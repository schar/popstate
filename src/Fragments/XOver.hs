{-# LANGUAGE QualifiedDo #-}

module Fragments.XOver
  ( module Fragments.XOver
  ) where

import Data.Foldable (for_)
import qualified Data.IMonad as M
import Data.Indexed
import Data.List.Extra (groupSort, intercalate, intersperse)
import Data.Scope
import Data.State
import Prelude hiding (all, any)

-- Model
-- =======================================

type E = String
type T = String

see :: E -> E -> T
see x y = y ++ " saw " ++ x

pet :: E -> E -> T
pet x y = y ++ " pet " ++ x

dog :: E -> E
dog x = x ++ "'s dog"

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


-- Grammar
-- =======================================

-- Scope IxApplicative, minimally </> and <\> (or </> and non-Scope LIFT). As
-- always, Scope's `η` is recoverable as `up . η`.

-- State IxMonad, but `up` instead of `>>=`. This entails that State effects
-- are exclusively propagated in Scope, which is assumed to have a L-R bias.
-- Unfettered access to the full State monad allows crossover. (The State
-- applicative operations alone do not, if also given a L-R bias, but they are
-- recoverable from the compositional primitives. Some of the definitions
-- below use State operations directly, for convenience.)

-- Vocabulary for generating, accessing drefs: ipush, lgets $ {lr, ll}^* lz
--
-- n ::= lz | lr n | ll n
--       ^^   ^^^^   ^^^^
--       DB indices  DB Gorn addresses
--
-- mpush and cpush are occasionally used in lieu of ipush for convenience
--
-- The popping action of lgets yields a dynamic analog of affine lambda calc,
-- appears potentially related to Rule H/I.
-- If we restrict to closed terms without active pushes -- State i i a -- we
-- have a dynamic analog of linear lambda calculus.

-- `down`, for ending derivations whose value is a 'closed term'.
down :: IxMonadState m => Scope r (m i (o, i) a) (m () o a) -> r
down = downWith evalClosed

evalClosed :: IxMonadState m => m () o a -> m i (o, i) a
evalClosed m = M.do
  i <- iget
  iput ()
  x <- m
  o <- iget
  iput (o, i)
  M.return x
-- e.g. evalClosed (State m) ==> State $ \i -> [(a, (o, i)) | (a, o) <- m ()]

-- evalClosed m = m >> iget >>= \o -> iput () >> m >>= \x -> iput o >> η x
-- this definition would not require gorn addresses, but it doesn't work in
-- Haskell: the two occurrences of `m` are required to have the same
-- monomorphic type. since `m` must be able to accept () as input state, `m`
-- is wrongly restricted to programs that *only* accept () as input.


-- Demonstration examples, and crossover
-- =======================================

-- "John or Bill"
jorb :: State i i E
jorb = disj ["john", "bill"]

-- "John or Bill saw Spot"
s1 :: State i (E, (E, i)) T
s1 = mpush jorb <\> η see </> ipush "spot"

-- "He pet it"
s2 :: State (E, (E, i)) i T
s2 = v1 <\> η pet </> v0

-- "John or Bill saw Spot; he pet it"
s3 :: State i i T
s3 = s1 <\> η conj </> s2

-- "some^u farmer who owns a^v donkey"
sfd'uv :: State i (E, (E, i)) E
sfd'uv = isum $ map (\(f, d) -> ipush d M.>> ipush f) own'

-- "some^u donkey they_v own"
sd'u_v :: State (a, (E, b)) (E, (a, b)) E
sd'u_v = M.do
  v <- v1
  isum [ipush d | (f, d) <- own', f == v]

-- "some^u farmer who saw it_v"
sf'u_v :: State (E, i) (E, i) E
sf'u_v = M.do
  v <- v0
  isum [ipush f | (f, d) <- see', d == v]

dyn
  :: ([T] -> T)                   -- | the underlying determiner
                                  --   (only possible if conservative, lol)
  -> ([T] -> T)                   -- | strength of the dynamicization:
                                  --   `all` yields strong readings; `any` yields weak readings
  -> State i j E                  -- | the restrictor as indefinite
                                  --
  -> Scope (State i i T) (State j k T) E
dyn det str p =
  Scope $ \c -> State $ \h -> [(det $ map (scope c) (restr h), h)]
  where
    restr h = groupSort (runState p h)
    scope c (x, bss) = str [true (c x) bs | bs <- bss]

everyS, everyW :: State i j E -> Scope (State i i T) (State j k T) E
everyS = dyn all all
everyW = dyn all any

-- "__ bought it from Bill"
vpss_v :: State (a, (E, i)) (E, (a, i)) (E -> T)
vpss_v = (buy <<$>> v1) </> ipush "bill"

-- "every farmer who owns a donkey bought it from Bill"
s4, s5 :: State i i T
s4 = down $ η <<$>> everyS sfd'uv <\> up vpss_v
s5 = down $ η <<$>> everyW sfd'uv <\> up vpss_v

-- "Bill sold it to __"
vpis_v :: E -> Scope (State (a, (E, i)) o T) (State (a, i) o T) T
vpis_v x = up $ η "bill" <\> (sell <<$>> v1) </> η x

-- "Bill sold it to every farmer who owns a donkey"
s6 :: Scope
        (State i i T)
        (State (E, (E, i)) j T)
        (State (a, (E, k)) ((), (a, k)) T)
--                          ^^ from initial, inside application of down
s6 = down . (η <<$>>) <<$>> (vpis_v <<$>> everyS sfd'uv)
--                           ^^^^^^^^^^^^^^^^^^^^^^^^^^
--                          higher-order inv scope tower
-- s6 can't be lowered again to bind the crossed-over pronoun:
-- > down s6 => • Couldn't match type ‘(a0, (E, k0))’ with ‘()’

-- [This remains consistent with the idea that scope islands are obligatorily
-- lowered. An unbound pronoun inside an island can take widest scope within
-- the island (there may be issues for S&B from such cases), or in certain
-- circumstances be shielded from `down` via an application of η.]

-- IxMonadState ipush lifted to Scope, for potential convenience
cpush :: IxMonadState m => Scope r (m i o b) a -> Scope r (m (a, i) o b) a
cpush m = M.do
  x <- m
  up $ ipush x

-- "sold every donkey they own to Bill"
vpb_u :: Scope
           (State (a, (E, b)) (a, (E, b)) T)
           (State (E, (E, (a, b))) o T)
           (E -> T)
vpb_u = (η sell </> everyS sd'u_v) </> cpush (η "bill")

-- "some farmer sold every donkey they own to Bill" (surface scope)
s7 :: State i (E, (E, i)) T
s7 = down cs
  where
    cs = η <<$>> cpush (cpush (up sf)) <\> vpb_u -- double push since 'they' is v1
    sf = disj ["john", "mary"]

-- "Bill sold some donkey to every farmer who saw it" (surface scope)
s8 :: State i (E, (E, i)) T
s8 = down cs
  where
    cs =
      η <<$>> cpush (η "bill") <\>
      ((η sell </> cpush (up sd)) </> everyS sf'u_v)
    sd = disj ["djm", "dm"]

-- "Mary^^ bought md1^ from Bill^"
s9 :: State i ((E, (E, ())), (E, i)) T
s9 = down $ up (ipush "mary") <\> η vp
  where
    vp x = η x <\> ((η buy </> ipush "md1") </> ipush "bill")
-- non-linear state

-- "Mary^ pet (her dog)^ and John^ pet it too
type Pi i = State (E,i) i E

s10 :: State i (((), ()), i) T
s10 = down $ down . (η <<$>>) <<$>> s
  where
    herdog = dog <<$>> v0
    p :: Scope (State g h b) (State (Pi i, g) h b) (Scope (State i j b') (State i j b') T)
    p = lift (up (ipush "mary")) <\\> (lift (lift pet) <//> (up <<$>> up (ipush herdog)))
    it :: Scope (State (Pi i, g) h b) (State g h b) (Scope (State (E,i) j b') (State i j b') E)
    it = up <<$>> up v0
    q :: Scope (State (Pi i, g) h b) (State g h b) (Scope (State i j b') (State i j b') T)
    q = lift (up (ipush "john")) <\\> (lift (lift pet) <//> it)
    s = p <\\> (lift (lift conj) <//> q)


-- Test
-- =======================================

test :: IO ()
test = do
  putStrLn ""
  sequence_ $ intersperse
    (putStrLn "")
    [ putStrLn "s1"  >> printUpdate s1  ()
    -- s2
    , putStrLn "s3"  >> printUpdate s3  ()
    , putStrLn "s4"  >> printUpdate s4  ()
    , putStrLn "s5"  >> printUpdate s5  ()
    -- s6
    , putStrLn "s7"  >> printUpdate s7  ()
    , putStrLn "s8"  >> printUpdate s8  ()
    , putStrLn "s9"  >> printUpdate s9  ()
    , putStrLn "s10" >> printUpdate s10 ()
    ]
  putStrLn ""

---}

printUpdate :: (Show a, Show o) => State i o a -> i -> IO ()
printUpdate m s = do
  putStrLn "{"
  for_ (runState m s) $ \(x, s') -> putStrLn $ show x ++ ";  " ++ show s'
  putStrLn "}"
