{-# LANGUAGE EmptyDataDecls, 
             MultiParamTypeClasses, 
             UndecidableInstances, 
             IncoherentInstances, 
             DataKinds,
             FunctionalDependencies,
             FlexibleContexts,
             RankNTypes,
             OverlappingInstances,
             TypeSynonymInstances,
             ScopedTypeVariables,
             UnicodeSyntax,
             GADTSyntax,
             GADTs,
             TypeFamilies,
             ConstraintKinds,
             InstanceSigs,
             GeneralizedNewtypeDeriving,
             StandaloneDeriving,
             ViewPatterns,
             FlexibleInstances #-}

module Music where

import Prelude hiding (negate)
import qualified Data.Map as Map
import Data.Ratio
import Data.Complex
import Control.Monad

import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace

import Util (interleave, iterateM,
             compose, member, intersection,
             remove, nd)

data AbstractPitch1 = AbstractPitch1 Degree Ficta deriving Eq

data AbstractPitch2 = AbstractPitch2 Name Accidental deriving Eq -- apply key


cent :: Freq
cent = (2 ** (1/12)) / 100

makeFreqList t = iterateM k g
  where g (b, s) = (h (b, s), tail s)
        h ((p, AbstractPitch3 f), s) = let (i, r) = head s
                                       in (transpose i p, AbstractPitch3 $ f * ((fromRational t) r))
        k (b, s) = [h (b, s)]
  -- t is our current tuning system, with its own base & steps
  -- s is where we currently are in the step list
  -- (i, r) (== head s) is the current interval (i) and multiplier (r) that we will use to get to the next pitch/frequency
  -- (p, f) (== b) is the current pitch/frequency


data AbstractPitch3 = AbstractPitch3 Freq deriving Eq -- apply tuning system

data Silence = Silence -- as a "pitch"

data AbstractInt1 = AbstractInt1 Skip deriving Eq
data AbstractInt2 = AbstractInt2 Quality Number deriving Eq
data AbstractInt3 = AbstractInt3 Freq deriving Eq

data AbstractDur1 = AbstractDur1 Pattern
data AbstractDur2 = AbstractDur2 (Ratio Integer)
data AbstractDur3 = AbstractDur3 Length

data Name = A | B | C | D | E | F | G | Up Name | Down Name deriving (Eq)
data Accidental = Na | Fl Accidental | Sh Accidental deriving Eq

sharp = Sh Na
flat = Fl Na
natural = Na

data Degree = TO | ST | ME | SD | DO | SM | LN | DUp Degree | DDown Degree deriving (Eq, Show)
data Ficta = Raise | Neutral | Lower deriving Eq
-- todo: something more convenient than this, e.g. R | N | L for ficta and U | D for octaves

applyFicta Raise Raise = Raise
applyFicta Raise Neutral = Raise
applyFicta Raise Lower = Neutral
applyFicta Lower Raise = Neutral
applyFicta Lower Neutral = Lower
applyFicta Lower Lower = Lower

instance Show Ficta where
  show Raise = "↑"
  show Neutral = "-"
  show Lower = "↓"

instance Show AbstractPitch1 where
  show (AbstractPitch1 d f) = (show d) ++ (show f)

instance Show Name where
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"
  show E = "E"
  show F = "F"
  show G = "G"
  show (Up n) = (show n) ++ "’"
  show (Down n) = (show n) ++ "‚"

instance Show Accidental where
  show Na = "♮"
  show (Fl Na) = "♭"
  show (Sh Na) = "♯"
  show (Fl (Fl Na)) = "𝄫"
  show (Sh (Sh Na)) = "𝄪"
  show (Fl (Fl a)) = '𝄫' : (show a)
  show (Sh (Sh a)) = '𝄪' : (show a)
  show (Fl a) = '♭' : (show a)
  show (Sh a) = '♯' : (show a)

instance Show Silence where
  show Silence = "rest"

instance Show AbstractPitch2 where
  show (AbstractPitch2 n a) = (show n) ++ (show a)

data Pattern = Single | Double Integer | Triple Integer

type Freq = Double

instance Bounded Freq where
  -- (limits of human hearing)
  minBound = 20
  maxBound = 20e3
  -- intended to help tuning procedures know when to switch direction.

-- instance Show Freq where
--   show (Freq f) = (show f) ++ " Hz"

instance Show AbstractPitch3 where
  show (AbstractPitch3 f) = showFreq f

instance Show AbstractInt3 where
  show (AbstractInt3 f) = showFreq f

showFreq = (++ " Hz") . show

type Length = Double

instance Show AbstractDur2 where
  show (AbstractDur2 r) = show r

instance Show AbstractDur3 where
  show (AbstractDur3 f) = (show f) ++ " ms"

data Skip = Fir | Sec | Thi | Fou | Fif | Six | Sev
          | Com Skip
          | Neg Skip
          deriving Eq

data Number = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh
            | Compound Number
            | Negative Number
            deriving Eq

instance Show Number where
  show Unison = "1"
  show Second = "2"
  show Third = "3"
  show Fourth = "4"
  show Fifth = "5"
  show Sixth = "6"
  show Seventh = "7"
  show (Compound l) = let x = ((read (show l)) :: Int)
                      in show (if x < 0 then (x - 7) else (x + 7))
  show (Negative l) = show (-1 * ((read (show l)) :: Int))

data Quality = Perf | Maj | Min | Aug Quality | Dim Quality deriving Eq

instance Show Quality where
  show Perf = "P"
  show Maj = "M"
  show Min = "m"
  show (Dim Perf) = "d"
  show (Dim Maj) = "d"
  show (Dim Min) = "d"
  show (Aug Perf) = "A"
  show (Aug Maj) = "A"
  show (Aug Min) = "A"
  show (Aug q) = "A" ++ (show q)
  show (Dim q) = "d" ++ (show q)

instance Show AbstractInt2 where
  show (AbstractInt2 q l) = (show q) ++ (show l)


------------------

class (Transpose p i, Duration d) => Note p i d where


class (Pitch p, Interval i) => Transpose p i | p -> i where
  transpose :: i -> p -> p
  interval :: p -> p -> i
  normalise :: p -> i -> p -> p

class Eq p => Pitch p where
  incr :: p -> p
  decr :: p -> p
  sharpen :: p -> p
  flatten :: p -> p

class Eq i => Interval i where
  unison :: i
  octave :: i
  invert :: i -> i
  negate :: i -> i
  add :: i -> i -> i
  sub :: i -> i -> i
  augment :: i -> i
  diminish :: i -> i
  grow :: i -> i
  shrink :: i -> i

class (Transpose p i) => Scale s p i | s -> p i where
  tonic :: s -> AbstractPitch2
  scale :: s -> [AbstractPitch2]
  applyScale :: s -> p -> AbstractPitch2

class Duration d where
--  stretch :: d -> d -> d

-- This method of specifying tuning systems will *not* allow different
-- tuning systems (instances of Tuning) to coexist in the same piece
-- of music. However, you may switch to a different version of the
-- same tuning system (e.g. centred on a differet
-- note/frequency). However, the method of *implementing* a Tuning
-- type is completely flexible, so if more configurability is needed,
-- just code a Tunable type that can radically change its properties,
-- e.g. by giving it various data constructors.

-- todo: make the output an (AbstractPitch3 Freq) rather than just a Freq.
class (Transpose p i) => Tuning t p i | t -> p i where
  -- Important: implementation of either tune or tuningStep is required!
  base :: t -> (p, AbstractPitch3) -- e.g. (A Na, 440)
--  steps :: t -> [(i, Ratio Integer)]  -- e.g. [(Perf Fifth, 3%3)]
  tune :: t -> p -> AbstractPitch3
--  freqList :: t -> [(p, AbstractPitch3)]
--  toFreq :: t -> n -> Freq

--  tune tuning pitch = p where (Just p) = lookup pitch (freqList tuning)

  -- So, if freqList doesn't eventually produce frequences for all
  -- notes, infinite loop fun ensues!

  -- By having tuning systems as *types*, rather than values per se,
  -- the intention is that the tuning can be represented very
  -- flexibly, as all it needs to do is have an instance t p declared
  -- for some pitch type p, rather than having some specific
  -- representation forced upon it.

  -- An instance could just give a frequency list as is -- or use this
  -- default implementation to generate it programmatically from base
  -- and steps.

-- fixme
--  freqList t = (base t) : (makeFreqList t (base t, cycle $ steps t))

---  freqList t =
---    (base t) :
---    (interleave
---     (makeFreqList t (base t, cycle $ steps t))
---     (makeFreqList t (base t, cycle $ negsteps)))
---    where negsteps = map (\(i, n) -> (negate i, 1/n)) (steps t)
  -- generate a list of frequencies consisting of:
  -- (all the notes below the base ... the base ... all the notes above the base)


class (Duration d) => Timing t d | t -> d where
  time :: t -> d -> AbstractDur3

--------------

instance Ord Name where
  m `compare` n = (fromEnum m) `compare` (fromEnum n)

instance Enum Name where
  fromEnum A = 0
  fromEnum B = 1
  fromEnum C = 2
  fromEnum D = 3
  fromEnum E = 4
  fromEnum F = 5
  fromEnum G = 6
  fromEnum (Up n) = (fromEnum n) + 7
  fromEnum (Down n) = (fromEnum n) - 7

  toEnum 0 = A
  toEnum 1 = B
  toEnum 2 = C
  toEnum 3 = D
  toEnum 4 = E
  toEnum 5 = F
  toEnum 6 = G
  toEnum n
    | (n < 0) = Down (toEnum (n + 7))
    | otherwise = Up (toEnum (n - 7))


instance Enum Degree where
  fromEnum TO = 0
  fromEnum ST = 1
  fromEnum ME = 2
  fromEnum SD = 3
  fromEnum DO = 4
  fromEnum SM = 5
  fromEnum LN = 6
  fromEnum (DUp d) = (fromEnum d) + 7
  fromEnum (DDown d) = (fromEnum d) - 7

  toEnum 0 = TO
  toEnum 1 = ST
  toEnum 2 = ME
  toEnum 3 = SD
  toEnum 4 = DO
  toEnum 5 = SM
  toEnum 6 = LN
  toEnum n
    | (n < 0) = DDown (toEnum (n + 7))
    | otherwise = DUp (toEnum (n - 7))


instance Enum Skip where
  fromEnum Fir = 0
  fromEnum Sec = 1
  fromEnum Thi = 2
  fromEnum Fou = 3
  fromEnum Fif = 4
  fromEnum Six = 5
  fromEnum Sev = 6
  fromEnum (Com s) = 7 + (fromEnum s)
  fromEnum (Neg s) = -1 * (fromEnum s)

  toEnum n
    | (n < 0) = Neg (toEnum (-1 * n))
    | (n == 7) = Com Fir
    | (n > 7) = Neg (toEnum (n - 7))
    | otherwise = toEnum' n
          where toEnum' 0 = Fir
                toEnum' 1 = Sec
                toEnum' 2 = Thi
                toEnum' 3 = Fou
                toEnum' 4 = Fif
                toEnum' 5 = Six
                toEnum' 6 = Sev

instance Ord Skip where
  m `compare` n = (fromEnum m) `compare` (fromEnum n)

instance Ord Number where
  m `compare` n = (fromEnum m) `compare` (fromEnum n)

instance Enum Number where
  fromEnum Unison = 0
  fromEnum Second = 1
  fromEnum Third = 2
  fromEnum Fourth = 3
  fromEnum Fifth = 4
  fromEnum Sixth = 5
  fromEnum Seventh = 6
  fromEnum (Compound l) = 7 + (fromEnum l)
  fromEnum (Negative l) = -1 * (fromEnum l)

  toEnum n
    | (n < 0) = Negative (toEnum (-1 * n))
    | (n == 7) = Compound Unison
    | (n > 7) = Compound (toEnum (n - 7))
    | otherwise = toEnum' n
          where toEnum' 0 = Unison
                toEnum' 1 = Second
                toEnum' 2 = Third
                toEnum' 3 = Fourth
                toEnum' 4 = Fifth
                toEnum' 5 = Sixth
                toEnum' 6 = Seventh



----------------------------------------------------
---- Data structures to store whole sequences of notes.

data Connector where
--  Connector :: VoiceName -> Phrase -> (AbstractNote p i d) -> Connector
  Connector :: VoiceName -> Phrase -> Connector
  Null :: Connector

instance Show Connector where
  show (Connector v p) = "(" ++ (show v) ++ ":" ++ (show p) ++ ")"
  show Null = "∎"
-- maybe different kind of connector for within-voice connections?

data AbstractNote p i d where
  AbstractPitch :: (Note p i d, Show p, Show d) =>
                  p -> d -> (AbstractNote p i d)
  AbstractInt :: (Note p i d, Show i, Show d) =>
                  i -> d -> (AbstractNote p i d)
  Rest :: (Duration d, Show d) => d -> (AbstractNote p i d)
  Conn :: Connector -> (AbstractNote p i d)

-- todo: fix to use proper ratio
showNote (AbstractDur2 (nd -> (2, 1))) = "𝅜"
showNote (AbstractDur2 (nd -> (1, 1))) = "𝅝"
showNote (AbstractDur2 (nd -> (1, 2))) = "𝅗𝅥"
showNote (AbstractDur2 (nd -> (1, 4))) = "𝅘𝅥"
showNote (AbstractDur2 (nd -> (1, 8))) = "𝅘𝅥𝅮"
showNote (AbstractDur2 r) = show r

showRest (AbstractDur2 (nd -> (2, 1))) = "𝄺"
showRest (AbstractDur2 (nd -> (1, 1))) = "𝄻"
showRest (AbstractDur2 (nd -> (1, 2))) = "𝄼"
showRest (AbstractDur2 (nd -> (1, 4))) = "𝄽"
showRest (AbstractDur2 (nd -> (1, 8))) = "𝄾"
showRest (AbstractDur2 r) = show r

type Note1 = AbstractNote AbstractPitch1 AbstractInt1 AbstractDur1
type Note2 = AbstractNote AbstractPitch2 AbstractInt2 AbstractDur2
type Note3 = AbstractNote AbstractPitch3 AbstractInt3 AbstractDur3

instance Show Note2 where
  show (AbstractPitch p d) = "{" ++ (show p) ++ " " ++ (showNote d) ++ "}"
  show (AbstractInt i d) = "{" ++ (show i) ++ " " ++ (showNote d) ++ "}"
  show (Rest d) = "{" ++ (showRest d) ++ "}"

instance Show (AbstractNote p i d) where
  show (AbstractPitch p d) = "Note{" ++ (show p) ++ " " ++ (show d) ++ "}"
  show (AbstractInt i d) = "Interval{" ++ (show i) ++ " " ++ (show d) ++ "}"
  show (Rest d) = "Rest{" ++ (show d) ++ "}"

-- deriving instance Show AbstractNote -- or this!


------------------------------------
-- Instances of classes defined above.

instance Note AbstractPitch1 AbstractInt1 AbstractDur1 where

instance Note AbstractPitch2 AbstractInt2 AbstractDur2 where

instance Note AbstractPitch3 AbstractInt3 AbstractDur3 where

instance Pitch AbstractPitch1 where
  sharpen (AbstractPitch1 d f) = AbstractPitch1 d (applyFicta Raise f)
  flatten (AbstractPitch1 d f) = AbstractPitch1 d (applyFicta Lower f)
  incr (AbstractPitch1 d f) = AbstractPitch1 (succ d) Neutral
  decr (AbstractPitch1 d f) = AbstractPitch1 (pred d) Neutral

instance Pitch AbstractPitch2 where
  sharpen (AbstractPitch2 n a) = toPitch $ (faPitch n a) + (1 ::+ 0)
  flatten (AbstractPitch2 n a) = toPitch $ (faPitch n a) - (1 ::+ 0)
  incr (AbstractPitch2 n a) = toPitch $ (faPitch n a) + (0 ::+ 1)
  decr (AbstractPitch2 n a) = toPitch $ (faPitch n a) - (0 ::+ 1)

instance Pitch AbstractPitch3 where
  sharpen (AbstractPitch3 f) = AbstractPitch3 (f * (1 + 50*cent))
  flatten (AbstractPitch3 f) = AbstractPitch3 (f * (1 - 50*cent))
  incr (AbstractPitch3 f) = AbstractPitch3 (f * (1 + 100*cent))
  decr (AbstractPitch3 f) = AbstractPitch3 (f * (1 - 100*cent))


instance Interval AbstractInt1 where
  add (AbstractInt1 s) (AbstractInt1 t) = AbstractInt1 $ toEnum $ (fromEnum s) + (fromEnum t)
  sub (AbstractInt1 s) (AbstractInt1 t) = AbstractInt1 $ toEnum $ (fromEnum s) - (fromEnum t)
  invert (AbstractInt1 s) = AbstractInt1 $ toEnum $ 7 - (fromEnum s)
  negate (AbstractInt1 s) = AbstractInt1 (propagateNegative s)
    where propagateNegative (Neg n) = n
          propagateNegative (Com n) = Com (Neg n)
          propagateNegative n = Neg n
  grow (AbstractInt1 s) = AbstractInt1 $ (toEnum . (+ 1) . fromEnum) s
  shrink (AbstractInt1 s) = AbstractInt1 $ (toEnum . (+(-1)) . fromEnum) s
  augment = id
  diminish = id
  unison = AbstractInt1 Fir
  octave = AbstractInt1 (Com Fir)


instance (Interval i) => AdditiveGroup i where
  zeroV = unison
  (^+^) = add
  negateV = negate

instance (Interval i) => VectorSpace i where
  type Scalar i = Int
  (*^) 0 i = zeroV
  (*^) s i
    | (s > 0) = i ^+^ ((s - 1) *^ i)
    | (s < 0) = (negateV i) ^+^ ((s + 1) *^ i)

-- instance VectorSpace AbstractInt2 where
  -- type Scalar AbstractInt2 = Int
  -- (*^) 0 i = zeroV
  -- (*^) s i
    -- | (s > 0) = i ^+^ ((s - 1) *^ i)
    -- | (s < 0) = (negateV i) ^+^ ((s + 1) *^ i)
-- 
-- instance VectorSpace AbstractInt3 where
  -- type Scalar AbstractInt3 = Int
  -- (*^) s (AbstractInt3 f) = AbstractInt3 $ f ** (fromIntegral s)

-- instance (Pitch p) => AffineSpace p where  -- not possible :-/
--   type (Diff p) = (Transpose p i) => i
--   (.-.) = interval
--   (.+^) = flip transpose

instance AffineSpace AbstractPitch1 where
  type Diff AbstractPitch1 = AbstractInt1
  (.-.) = interval
  (.+^) = flip transpose

instance AffineSpace AbstractPitch2 where
  type Diff AbstractPitch2 = AbstractInt2
  (.-.) = interval
  (.+^) = flip transpose

instance AffineSpace AbstractPitch3 where
  type Diff AbstractPitch3 = AbstractInt3
  (.-.) = interval
  (.+^) = flip transpose


-- todo: make pitches an affine space (intervals are the difference type), using AffineSpace

data FreeAbelian = Int ::+ Int deriving (Show, Eq)
-- We're using the type 'FreeAbelian' to represent (n * A1, m * d2).
--
-- Essentially, intervals form a free Abelian group G = {(n*e_1,m*e_2)
-- | n,m in Z} where e_1 = (1,0) and e_2 = (0,1) are two possible
-- elements that can be used as a basis (generators_ for the group.
-- An interval ratio is then defined as (A1)^n * (d2)^m.
--
-- Note that, in Pythagorean tuning, d2 is comma^(-1) (in negative
-- direction), and m2 + comma = A1 (i.e. m2 < A1).
--
-- Could use P8 & P5 as generators, or m2 & d2, or whatever really.
--
-- Todo: use signed multiset instead? Probably overkill, even though
-- that's technically what free Abelian groups "are".

instance Ord FreeAbelian where
  (_ ::+ a) <= (_ ::+ b) = a <= b
  -- It's the second element (m*d2) that gives rise to the *name* of
  -- the interval (Unison, Second, Third etc.), so that's what we're
  -- most likely going to want to compare on.

instance Num FreeAbelian where
  (a ::+ b) + (c ::+ d) = (a + c) ::+ (b + d)
  (a ::+ b) - (c ::+ d) = (a - c) ::+ (b - d)
  (a ::+ b) * (c ::+ d) = (a*c - b*d) ::+ (b*c + a*d) -- maybe...
  fromInteger n = (fromIntegral n) ::+ 0
  abs (a ::+ b) = undefined -- (cannot give the absolute magnitude of a group element until we know what uning system we're using)
  signum (a ::+ b) = undefined

faInt :: Quality -> Number -> FreeAbelian

-- i.e. intervals as elements of the free Abelian group

faInt Perf Unison = 0 ::+ 0
faInt (Aug Perf) Unison = 1 ::+ 0
faInt (Dim Min) Second = 0 ::+ 1

faInt Min Second = (faInt (Dim Min) Second) + (faInt (Aug Perf) Unison)
faInt Maj Second = (faInt Min Second) + (faInt (Aug Perf) Unison)
faInt Min Third = (faInt Maj Second) + (faInt Min Second)
faInt Maj Third = (faInt Maj Second) + (faInt Maj Second)
faInt Perf Fourth = (faInt Maj Third) + (faInt Min Second)
faInt Perf Fifth = (faInt Perf Fourth) + (faInt Maj Second)
faInt Min Sixth = (faInt Perf Fifth) + (faInt Min Second)
faInt Maj Sixth = (faInt Perf Fifth) + (faInt Maj Second)
faInt Min Seventh = (faInt Maj Sixth) + (faInt Min Second)
faInt Maj Seventh = (faInt Maj Sixth) + (faInt Maj Second)
faInt Perf (Compound Unison) = (faInt Maj Seventh) + (faInt Min Second)

faInt q (Compound n) = (faInt Perf (Compound Unison)) + (faInt q n)
faInt q (Negative n) = (faInt Perf Unison) - (faInt q n)

faInt (Dim q) n = (faInt q n) - (faInt (Aug Perf) Unison)
faInt (Aug q) n = (faInt q n) + (faInt (Aug Perf) Unison)

toInterval (a ::+ d) = AbstractInt2 (faIntToQual (a ::+ d)) (toEnum d)
intToFa (AbstractInt2 q n) = faInt q n

faIntToQual (n ::+ m)
  | (n < 0) && (m == 0) = Dim (faIntToQual ((n + 1) ::+ m))
  | (n ::+ m) == (0 ::+ 0) = Perf
  | (n > 0) && (m == 0) = Aug (faIntToQual ((n - 1) ::+ m))
  | (n < 1) && (m == 1) = Dim (faIntToQual ((n + 1) ::+ m))
  | (n ::+ m) == (1 ::+ 1) = Min
  | (n ::+ m) == (2 ::+ 1) = Maj
  | (n > 2) && (m == 1) = Aug (faIntToQual ((n - 1) ::+ m))
  | (n < 3) && (m == 2) = Dim (faIntToQual ((n + 1) ::+ m))
  | (n ::+ m) == (3 ::+ 2) = Min
  | (n ::+ m) == (4 ::+ 2) = Maj
  | (n > 4) && (m == 2) = Aug (faIntToQual ((n - 1) ::+ m))
  | (n < 5) && (m == 3) = Dim (faIntToQual ((n + 1) ::+ m))
  | (n ::+ m) == (5 ::+ 3) = Perf
  | (n > 5) && (m == 3) = Aug (faIntToQual ((n - 1) ::+ m))
  | (n < 7) && (m == 4) = Dim (faIntToQual ((n + 1) ::+ m))
  | (n ::+ m) == (7 ::+ 4) = Perf
  | (n > 7) && (m == 4) = Aug (faIntToQual ((n - 1) ::+ m))
  | (n < 8) && (m == 5) = Dim (faIntToQual ((n + 1) ::+ m))
  | (n ::+ m) == (8 ::+ 5) = Min
  | (n ::+ m) == (9 ::+ 5) = Maj
  | (n > 9) && (m == 5) = Aug (faIntToQual ((n - 1) ::+ m))
  | (n < 10) && (m == 6) = Dim (faIntToQual ((n + 1) ::+ m))
  | (n ::+ m) == (10 ::+ 6) = Min
  | (n ::+ m) == (11 ::+ 6) = Maj
  | (n > 11) && (m == 6) = Aug (faIntToQual ((n - 1) ::+ m))
  | (n < 12) && (m == 7) = Dim (faIntToQual ((n + 1) ::+ m))
  | (n ::+ m) == (12 ::+ 7) = Perf
  | (n > 12) && (m == 7) = Aug (faIntToQual ((n - 1) ::+ m))

-- todo: fixme so less weird
--  | (n < 0) || (m < 0) = faIntToQual ((n + 12) ::+ (m + 7))
  | (n > 12) || (m > 7) = faIntToQual ((n - 12) ::+ (m - 7))
  | (n < 0) || (m < 0) = faIntToQual ((-n) ::+ (-m))

instance Interval AbstractInt2 where
  add (AbstractInt2 q n) (AbstractInt2 p m) = toInterval $ (faInt q n) + (faInt p m)
  sub (AbstractInt2 q n) (AbstractInt2 p m) = toInterval $ (faInt q n) - (faInt p m)
  invert (AbstractInt2 q n) = toInterval $ (faInt Perf (Compound Unison)) - (faInt q n)
  negate (AbstractInt2 q n) = AbstractInt2 q (propagateNegative n)
    where propagateNegative (Negative n) = n
          propagateNegative (Compound n) = Compound (Negative n)
          propagateNegative n = Negative n
  augment (AbstractInt2 q n) = toInterval $ (faInt q n) + (1 ::+ 0)
  diminish (AbstractInt2 q n) = toInterval $ (faInt q n) - (1 ::+ 0)
  grow (AbstractInt2 q n) = toInterval $ (faInt q n) + (1 ::+ 1)
  shrink (AbstractInt2 q n) = toInterval $ (faInt q n) - (1 ::+ 1)
  octave = AbstractInt2 Perf (Compound Unison)
  unison = AbstractInt2 Perf Unison




-- fixme: AbstractInt3 should always measure *ratio*s
instance Interval AbstractInt3 where
  add (AbstractInt3 f) (AbstractInt3 g) = AbstractInt3 (f*g)
  sub (AbstractInt3 f) (AbstractInt3 g) = AbstractInt3 (f/g)
  invert (AbstractInt3 f) = AbstractInt3 (2/f) -- maybe?
  negate (AbstractInt3 f) = AbstractInt3 (1/f)
  augment (AbstractInt3 f) = AbstractInt3 (f * (1 + 50*cent)) -- ????
  diminish (AbstractInt3 f) = AbstractInt3 (f * (1 - 50*cent))
  grow (AbstractInt3 f) = AbstractInt3 (f * (1 + 100*cent))
  shrink (AbstractInt3 f) = AbstractInt3 (f * (1 - 100*cent))
  unison = AbstractInt3 1
  octave = AbstractInt3 2

instance Transpose AbstractPitch1 AbstractInt1 where
  transpose (AbstractInt1 s) (AbstractPitch1 d f) = AbstractPitch1 (toEnum $ (fromEnum s) + (fromEnum d)) Neutral
  interval (AbstractPitch1 d f) (AbstractPitch1 d' f') = AbstractInt1 $ toEnum $ (fromEnum d') - (fromEnum d)
  normalise (AbstractPitch1 d f) (AbstractInt1 s) (AbstractPitch1 e g)
    | s < (Com Fir) = undefined
    | (current >= lower) && (current < upper) = AbstractPitch1 d f
    | current < lower = normalise (AbstractPitch1 d f) (AbstractInt1 s) (transpose (AbstractInt1 (Com Fir)) (AbstractPitch1 e g))
    | otherwise = normalise (AbstractPitch1 d f) (AbstractInt1 s) (transpose (AbstractInt1 (Neg (Com Fir))) (AbstractPitch1 e g))
    where lower = fromEnum d
          upper = lower + (fromEnum s)
          current = fromEnum e



faPitch :: Name -> Accidental -> FreeAbelian

-- i.e. pitches as elements of the free Abelian group

-- all pitches are measured relative to middle-A-natural

faPitch A Na = 0 ::+ 0
faPitch B Na = (faPitch A Na) + (faInt Maj Second)
faPitch C Na = (faPitch B Na) + (faInt Min Second)
faPitch D Na = (faPitch C Na) + (faInt Maj Second)
faPitch E Na = (faPitch D Na) + (faInt Maj Second)
faPitch F Na = (faPitch E Na) + (faInt Min Second)
faPitch G Na = (faPitch F Na) + (faInt Maj Second)

faPitch (Up n) Na = (faPitch n Na) + (faInt Perf (Compound Unison))
faPitch (Down n) Na = (faPitch n Na) - (faInt Perf (Compound Unison))

faPitch n (Fl a) = (faPitch n a) - (1 ::+ 0)
faPitch n (Sh a) = (faPitch n a) + (1 ::+ 0)

toPitch :: FreeAbelian -> AbstractPitch2
toPitch (n ::+ m) = AbstractPitch2 (toEnum m) (faIntToAcc (n ::+ m))

pitchToFa (AbstractPitch2 n a) = faPitch n a

faIntToAcc (n ::+ m)
  | (n < 0) && (m == 0) = Fl (faIntToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (0 ::+ 0) = Na
  | (n > 0) && (m == 0) = Sh (faIntToAcc ((n - 1) ::+ m))
  | (n < 2) && (m == 1) = Fl (faIntToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (2 ::+ 1) = Na
  | (n > 2) && (m == 1) = Sh (faIntToAcc ((n - 1) ::+ m))
  | (n < 3) && (m == 2) = Fl (faIntToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (3 ::+ 2) = Na
  | (n > 3) && (m == 2) = Sh (faIntToAcc ((n - 1) ::+ m))
  | (n < 5) && (m == 3) = Fl (faIntToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (5 ::+ 3) = Na
  | (n > 5) && (m == 3) = Sh (faIntToAcc ((n - 1) ::+ m))
  | (n < 7) && (m == 4) = Fl (faIntToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (7 ::+ 4) = Na
  | (n > 7) && (m == 4) = Sh (faIntToAcc ((n - 1) ::+ m))
  | (n < 8) && (m == 5) = Fl (faIntToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (8 ::+ 5) = Na
  | (n > 8) && (m == 5) = Sh (faIntToAcc ((n - 1) ::+ m))
  | (n < 10) && (m == 6) = Fl (faIntToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (10 ::+ 6) = Na
  | (n > 10) && (m == 6) = Sh (faIntToAcc ((n - 1) ::+ m))
  | (n < 12) && (m == 7) = Fl (faIntToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (12 ::+ 7) = Na
  | (n > 12) && (m == 7) = Sh (faIntToAcc ((n - 1) ::+ m))

  | (n < 0) || (m < 0) = faIntToAcc ((n + 12) ::+ (m + 7))
  | (n > 12) || (m > 7) = faIntToAcc ((n - 12) ::+ (m - 7))

instance Transpose AbstractPitch2 AbstractInt2 where
  transpose (AbstractInt2 q i) (AbstractPitch2 n a) = toPitch $ (faPitch n a) + (faInt q i)

  interval (AbstractPitch2 n a) (AbstractPitch2 p b) = toInterval $ (faPitch p b) - (faPitch n a)

  normalise (AbstractPitch2 n a) (AbstractInt2 q i) (AbstractPitch2 p b)
    | (faInt q i) < (faInt Perf (Compound Unison)) = undefined
    | (current >= lower) && (current < upper) = AbstractPitch2 p b
    | current < lower = normalise (AbstractPitch2 n a) (AbstractInt2 q i) (transpose (AbstractInt2 Perf (Compound Unison)) (AbstractPitch2 p b))
    | otherwise = normalise (AbstractPitch2 n a) (AbstractInt2 q i) (transpose (AbstractInt2 Perf (Negative (Compound Unison))) (AbstractPitch2 p b))
    where lower = faPitch n a
          upper = lower + (faInt q i)
          current = faPitch p b


-- todo: measure AbstractInt3 in cents
instance Transpose AbstractPitch3 AbstractInt3 where
  transpose (AbstractInt3 df) (AbstractPitch3 f) = AbstractPitch3 (f + df)
  interval (AbstractPitch3 f) (AbstractPitch3 g) = AbstractInt3 (g - f)
  normalise (AbstractPitch3 f) (AbstractInt3 df) (AbstractPitch3 g)
    | upper/lower <= 0.5 = undefined
    | (current >= lower) && (current < upper) = AbstractPitch3 g
    | current < lower = normalise (AbstractPitch3 f) (AbstractInt3 df) (AbstractPitch3 (g*2))
    | otherwise = normalise (AbstractPitch3 f) (AbstractInt3 df) (AbstractPitch3 (g/2))
    where lower = f
          upper = f + df
          current = g


instance Duration AbstractDur1 where

instance Duration AbstractDur2 where

instance Duration AbstractDur3 where
--  stretch (AbstractDur3 f) (AbstractDur3 df) = (AbstractDur3 (f * df))


data Metronome = Metronome Int

instance Show Metronome where
  show (Metronome n) = "𝅘𝅥 = " ++ (show n)

instance Timing Metronome AbstractDur2 where
  time (Metronome n) (AbstractDur2 r) = AbstractDur3 (240000/(fromIntegral n) * (fromRational r))


-- 60 bpm = 15 sbpm
-- 1 sb = 60s/15 = 60s/(bpm/4) = 60000 ms/ (bpm/4)




-------- handy shortcuts:

pitch :: Name -> Accidental -> AbstractPitch2
pitch n a = AbstractPitch2 n a

freq :: Double -> AbstractPitch3
freq f = AbstractPitch3 f

int :: Quality -> Number -> AbstractInt2
int q n = AbstractInt2 q n

note :: AbstractPitch2 -> AbstractDur2 -> Note2
note p d = AbstractPitch p d

relnote :: AbstractInt2 -> AbstractDur2 -> Note2
relnote i d = AbstractInt i d

rhythm a b = AbstractDur2 (a % b)
crotchet = rhythm 1 4
minim = rhythm 1 2
semibreve = rhythm 1 1
quaver = rhythm 1 8
semiquaver = rhythm 1 16

dotted (AbstractDur2 r) = AbstractDur2 (r * (3%2))
ddotted (AbstractDur2 r) = AbstractDur2 (r * (7%4))
tdotted (AbstractDur2 r) = AbstractDur2 (r * (15%8))

tie (AbstractDur2 r) (AbstractDur2 s) = AbstractDur2 (r + s)

noteToSound :: (Tuning t AbstractPitch2 AbstractInt2, Timing i AbstractDur2) => t -> i -> Note2 -> Note3
noteToSound tuning timing (AbstractPitch p d) = AbstractPitch (tune tuning p) (time timing d)
noteToSound _ timing (Rest d) = Rest (time timing d)

-- rest = AbstractNote Silence mi

-- data Phrase p i d where
--   makePhrase :: Note p i d => Phrase

-- data Phrase p i d where
-- --  Notes :: [Note] -> Phrase
-- --  Phrase :: (Transpose p i, Duration d) => [(AbstractNote p i d)] -> Phrase
  -- Phrase :: [(AbstractNote p i d)] -> Phrase p i d
  -- Blank :: Phrase p i d
-- 
-- -- deriving instance Show Phrase
-- 
-- instance Show (Phrase p i d) where
  -- show (Phrase p) = show p
  

-- data AbstractPhrase p i d = Phrase [(AbstractNote p i d)]
                          -- | Blank
                          -- deriving Show

data AbstractPhrase n where -- a phrase tied to a particular type of note
  AbstractPhrase :: (Note p i d) => [AbstractNote p i d] -> AbstractPhrase (AbstractNote p i d)

-- instance Functor AbstractPhrase where
--  fmap = mapPhrase -- not working :-(

mapPhrase :: (Note p i d, Note p' i' d') =>
             (AbstractNote p i d -> AbstractNote p' i' d') -> AbstractPhrase (AbstractNote p i d) -> AbstractPhrase (AbstractNote p' i' d')
mapPhrase f p = listToPhrase $ map f (phraseToList p)

concatPhrase :: (Note p i d) => AbstractPhrase (AbstractNote p i d) -> AbstractPhrase (AbstractNote p i d) -> AbstractPhrase (AbstractNote p i d)
concatPhrase a b = listToPhrase $ (phraseToList a) ++ (phraseToList b)

nullPhrase :: (Note p i d) => AbstractPhrase (AbstractNote p i d)
nullPhrase = AbstractPhrase []

phraseToList :: (Note p i d) => AbstractPhrase (AbstractNote p i d) -> [AbstractNote p i d]
phraseToList (AbstractPhrase ns) = ns

listToPhrase :: (Note p i d) => [AbstractNote p i d] -> AbstractPhrase (AbstractNote p i d)
listToPhrase ns = (AbstractPhrase ns)

absolute :: (Transpose p i, Duration d) => AbstractPhrase (AbstractNote p i d) -> AbstractPhrase (AbstractNote p i d)
absolute (AbstractPhrase (n:ns)) = AbstractPhrase (n:(absolute' n ns))
  where absolute' _ ((AbstractPitch p d):notes) = (AbstractPitch p d) : (absolute' (AbstractPitch p d) notes)
        absolute' base@(AbstractPitch p _) ((AbstractInt i d):notes) = (AbstractPitch (transpose i p) d) : (absolute' base notes)
        absolute' base ((Rest d):notes) = (Rest d) : (absolute' base notes)
        absolute' _ [] = []

instance Show (AbstractNote p i d) => Show (AbstractPhrase (AbstractNote p i d)) where
  show (AbstractPhrase x) = show x

data Phrase where -- a generic phrase that subsumes AbstractPhrases
  Phrase :: Show (AbstractNote p i d) => (AbstractPhrase (AbstractNote p i d)) -> Connector -> Phrase
  Blank :: Connector -> Phrase

instance Show Phrase where
  show (Phrase p c) = (show p) ++ "->" ++ (show c)
  show (Blank c) = "…" ++ "->" ++ (show c)

makePhrase :: [Note2] -> Connector -> Phrase
makePhrase x c = Phrase (AbstractPhrase x) c

data Voice = Voice [Phrase]
           deriving Show

data VoiceName = V Integer deriving (Eq, Ord)

instance Show VoiceName where
  show (V n) = "v" ++ (show n)

data Music = Voices (Map.Map VoiceName Voice)
           | Start Phrase
           deriving Show


------------------------




