{-# LANGUAGE OverlappingInstances,
             EmptyDataDecls, 
             MultiParamTypeClasses, 
             DataKinds,
             FunctionalDependencies,
             FlexibleContexts,
             RankNTypes,
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
             UndecidableInstances, 
             FlexibleInstances #-}

             -- ImpredicativeTypes,
             -- ExplicitForAll,
             -- IncoherentInstances, 


module Music where
-- Todo: make this file literate Haskell (.lhs), due to the large
-- number of long comments.

-- Also todo: split this file out a bit, it's now over 1000 lines!

import Prelude hiding (negate)
import qualified Data.Map as Map
import Data.Ratio
import Data.Complex
import Control.Monad

import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace
import Data.Semigroup hiding (Min)

-- import qualified Data.Music.Lilypond as L

import Util (interleave, iterateM,
             compose, member, intersection,
             remove, nd, foldSG, under, divides,
             listDiff, uniq)



-------- Main type declarations:

data AbstractPitch1 = AbstractPitch1 Degree Ficta deriving Eq -- scale degrees
data AbstractPitch2 = AbstractPitch2 Name Accidental -- pitch
data AbstractPitch3 = AbstractPitch3 Freq deriving Eq -- frequencies
-- type Figuring = AbstractInt1 -- speculative ... for figured bass

data AbstractInt1 = AbstractInt1 Skip Ficta deriving (Eq, Show) -- "intervals" between scale degrees
data AbstractInt2 = AbstractInt2 Quality Number -- intervals between ordinary pitches
data AbstractInt3 = AbstractInt3 FreqRat deriving Eq -- ratios between frequencis

data AbstractDur1 = AbstractDur1 MDur deriving (Eq, Show, Ord) -- prolations
data AbstractDur2 = AbstractDur2 (Ratio Int) deriving (Eq, Ord) -- note durations
data AbstractDur3 = AbstractDur3 Length deriving (Eq, Ord) -- actual duration in milliseconds

data Name = A | B | C | D | E | F | G | Up Name | Down Name deriving (Eq)
data Accidental = Na | Fl Accidental | Sh Accidental deriving Eq

-- data Degree = Ut | Re | Mi | Fa | Sol | La | LN | DUp Degree | DDown Degree deriving (Eq, Show)
data Degree = Ut | Re | Mi | Fa | Sol | La | Si deriving (Eq, Show)
data Ficta = Raise | Neutral | Lower deriving Eq
-- todo: something more convenient than this, e.g. R | N | L for ficta and U | D for octaves

type FreqRat = Double -- ratio of frequencies
type Freq = Double -- frequency in Hz
type Length = Double

-- see also https://en.wikipedia.org/wiki/Rhythmic_mode
data MDur = Mx | Ln | Br | Sb | Mn | Sm | Ff | Sf | MTie MDur MDur | Punctus MDur
          deriving (Eq, Show)

data Skip = Fir | Sec | Thi | Fou | Fif | Six | Sev
          | Com Skip
          | Neg Skip
          deriving (Eq, Show)

data Number = Unison | Second | Third | Fourth | Fifth | Sixth | Seventh
            | Compound Number
            | Negative Number
            deriving Eq

data Quality = Perf | Maj | Min | Aug Quality | Dim Quality deriving Eq


data Metronome = Metronome Int deriving Eq


-- A note can be one of:
-- AbstractPitch (an absolute pitch)     \
-- AbstractInt (a relative pitch)         | these have durations and pitches
-- Rest (just a duration, no sound)      /
-- Conn (a pointer to another phrase that starts simultaneously with the following 'real' note)  \  these have neither durations
-- Dir (a convenient way of putting 'inaudible' symbols/other commands in the output)            /       nor pitches.

data AbstractNote p i d where
  AbstractPitch :: (Note p i d, Show p, Show d) => p -> d -> (AbstractNote p i d)
  AbstractInt :: (Note p i d, Show i, Show d) => i -> d -> (AbstractNote p i d)
  Rest :: (Duration d, Show d) => d -> (AbstractNote p i d)
  Conn :: (Show p, Show i, Show d, Note p i d) => AbstractPhrase (AbstractNote p i d) -> (AbstractNote p i d)
  Dir :: (Note p i d) => (Directive p i d) -> AbstractNote p i d

deriving instance Eq (AbstractNote p i d)


coerceNote :: (Note p i d, Note p' i' d') => AbstractNote p i d -> AbstractNote p' i' d'
-- coerceNote (Dir d) = Dir d
coerceNote n = error $ "Don't know how to coerce " ++ (show n)

isConn (Conn _) = True
isConn _ = False

isNote (AbstractPitch _ _) = True
isNote (AbstractInt _ _) = True
isNote (Rest _) = True
isNote _ = False


data Directive p i d where
  -- Tempo :: (Note p i d, Timing t d) => t -> Directive p i d -- hmmm
  Tempo :: (Note p i d) => Metronome -> Directive p i d
  -- Retune :: (Note p i d, Tuning t p i) => t -> Directive p i d -- hmmm
  Figuring :: (Note p i d) => [i] -> Directive p i d

deriving instance Eq (Directive p i d)

deriving instance Show (Directive p i d)

-- Note: we *could* make Conn look like this:
--     Conn :: (Note p i d, Note p' i' d') => AbstractPhrase (AbstractNote p' i' d') -> (AbstractNote p i d)
-- because ideally we'd like Conn to be able to point to an
-- AbstractPhrase of arbitrary type; but this breaks mapPhrase (and
-- everything else) due to GADTs being hard.


-- A phrase of a particular type of note. The fact that notes can
-- themselves be pointers to other phrases (see the Conn constructor)
-- makes this a *bit* like a rose tree.
data AbstractPhrase n where
  AbstractPhrase :: (Note p i d) => [AbstractNote p i d] -> AbstractPhrase (AbstractNote p i d)

deriving instance Eq (AbstractPhrase n)


-- A collection of phrases ('Voices') forms a piece of music -- or,
-- alternatively, one single phrase ('Start') starts the whole piece
-- off, and the other phrases split of from it using Conn
-- constructors,
data Music n where
  Start :: (Show p, Show i, Show d, Note p i d) => AbstractPhrase (AbstractNote p i d) -> Music (AbstractNote p i d)
  Voices :: (Show p, Show i, Show d, Note p i d) => [AbstractPhrase (AbstractNote p i d)] -> Music (AbstractNote p i d)

deriving instance Show (Music n)

-- Ignore the order of voices when comparing music values
instance Eq (Music n) where
    (Voices vs) == (Voices vs') = null $ vs `listDiff` vs'
    (Voices vs) == p@(Start _) = null $ vs `listDiff` (voiceList $ explodeVoices p)
    p@(Start _) == vs@(Voices _) = vs == p
    p@(Start _) == p'@(Start _) = (explodeVoices p) == (explodeVoices p')

voiceList :: (Note p i d, n ~ AbstractNote p i d) => Music n -> [AbstractPhrase n]
voiceList (Voices v) = v
voiceList (Start m) = voiceList $ explodeVoices (Start m)

-------- Type instances of the above types for Ord, Eq, Show etc.

instance Ord AbstractInt1 where
  compare = compare `under` (\(AbstractInt1 s _) -> fromEnum s)

instance Ord AbstractPitch1 where
  compare = compare `under` (\(AbstractPitch1 d _) -> fromEnum d)

instance Ord AbstractInt2 where
  compare = compare `under` (\(AbstractInt2 _ n) -> fromEnum n)

instance Ord AbstractPitch2 where
  compare = compare `under` (\(AbstractPitch2 n _) -> fromEnum n)

instance Ord AbstractInt3 where
  compare = compare `under` (\(AbstractInt3 f) -> f)

instance Ord AbstractPitch3 where
  compare = compare `under` (\(AbstractPitch3 f) -> f)

instance Ord MDur where
  compare _ _ = error "Not implemented yet"

instance Show AbstractPitch1 where
  show (AbstractPitch1 d f) = (show d) ++ (show f)

instance Show Ficta where
  show Raise = "↑"
  show Neutral = "-"
  show Lower = "↓"

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

instance Show AbstractPitch2 where
  show (AbstractPitch2 n a) = (show n) ++ (show a)

instance Bounded Freq where
  -- (limits of human hearing)
  minBound = 20
  maxBound = 20e3

instance Show AbstractPitch3 where
  show (AbstractPitch3 f) = showFreq f

instance Show AbstractInt3 where
  show (AbstractInt3 f) = show f

showFreq = (++ " Hz") . show

instance Show AbstractDur2 where
  show (AbstractDur2 r) = show r

instance Show AbstractDur3 where
  show (AbstractDur3 f) = (show f) ++ " ms"

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
  show (Aug q) = 'A':(show q)
  show (Dim q) = 'd':(show q)

instance Show AbstractInt2 where
  show (AbstractInt2 q l) = (show q) ++ (show l)

instance Show Metronome where
  show (Metronome n) = "𝅘𝅥 = " ++ (show n)


------------------

class (Transpose p i, Duration d) => Note p i d where
  phrase :: [AbstractNote p i d] -> AbstractPhrase (AbstractNote p i d)
  phrase ns = AbstractPhrase ns
  note :: p -> d -> AbstractNote p i d
  note p d = AbstractPitch p d
  rest :: d -> AbstractNote p i d
  rest d = Rest d


class (Pitch p, Interval i, AffineSpace p, VectorSpace i) => Transpose p i | p -> i, i -> p where
  transpose :: i -> p -> p
  interval :: p -> p -> i
  normalise :: p -> i -> p -> p
  normalise _ _ _ = undefined

class (Show p, Eq p, Ord p, AffineSpace p) => Pitch p where
  incr :: p -> p
  decr :: p -> p
  sharpen :: p -> p
  flatten :: p -> p
  middle :: p

class (Show i, Eq i, AdditiveGroup i) => Interval i where
  unison :: i
  octave :: i
  invert :: i -> i
  invert i = sub octave i
  negate :: i -> i
  negate i = sub unison i
  add :: i -> i -> i
  sub :: i -> i -> i
  augment :: i -> i
  diminish :: i -> i
  grow :: i -> i
  shrink :: i -> i

class (Transpose p i) => Scale s p i | s -> p i where
  tonic :: s -> AbstractPitch2
  tonic = head . scale
  final :: s -> AbstractPitch2
  final s = (tonic s) .+^ octave
  scale :: s -> [AbstractPitch2]
  applyScale :: s -> p -> AbstractPitch2

class (Semigroup d, Show d, Eq d, Ord d) => Duration d where
  unit :: d           -- The 'base' duration that time signatures refer to
  combine :: d -> d -> d
  zeroD :: d          -- Optional, obviously if it exists it should be the identity for combine
  subD :: d -> d -> d -- Optional, obviously subD d d = zeroD
  tie :: d -> d -> d
  tie = combine
  showDur :: d -> String
  showDur d = show d
  showRest :: d -> String
  showRest d = show d


class Mensuration m where
  mensurate :: m -> AbstractDur1 -> AbstractDur2


-- Essentially a tuning system is anything that implements 'tuneInt'
-- (or 'tune' and 'tuneInt'). But, other than that, it's up to you. If
-- more configurability is needed, just write a Tuning type whose
-- constructor has lots of parameters -- or whose implementation of
-- 'tune' does something more complicated that just utilise 'tuneInt',
-- etc.  (see DummyTuning in Tuning.hs for a facetious example)

class (Transpose p i) => Tuning t p i | t -> p i where
  -- Important: implementation of either tune or tuneInt is required!
  base :: t -> (p, AbstractPitch3) -- e.g. (A Na, 440)
  tuneInt :: t -> i -> AbstractInt3
  tune :: t -> p -> AbstractPitch3
  tune t p' = let (p, r) = base t
              in r .+^ (tuneInt t (interval p p'))
  tuneNote :: Note p i d => t -> AbstractNote p i d -> AbstractNote AbstractPitch3 AbstractInt3 d
  tuneNote t (AbstractPitch p d) = AbstractPitch (tune t p) d
  tuneNote t (AbstractInt i d) = AbstractInt (tuneInt t i) d
  tuneNote _ (Rest d) = Rest d
  tuneNote _ d = coerceNote d

-- Any way of specifying a concrete realisation of tempo -- some magic
-- involving the IO monad may allow for accelerations etc.

class (Duration d, Eq d, Eq t) => Timing t d | t -> d where
  time :: t -> d -> AbstractDur3
  timeNote :: Note p i d => t -> AbstractNote p i d -> AbstractNote p i AbstractDur3
  timeNote t (AbstractPitch p d) = AbstractPitch p (time t d)
  timeNote t (AbstractInt i d) = AbstractInt i (time t d)
  timeNote t (Rest d) = Rest (time t d)
  timeNote _ d = coerceNote d

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
  fromEnum Ut  = 0
  fromEnum Re  = 1
  fromEnum Mi  = 2
  fromEnum Fa  = 3
  fromEnum Sol = 4
  fromEnum La  = 5
  fromEnum Si  = 6
--   fromEnum (DUp d) = (fromEnum d) + 7
--   fromEnum (DDown d) = (fromEnum d) - 7

  toEnum 0 = Ut 
  toEnum 1 = Re 
  toEnum 2 = Mi 
  toEnum 3 = Fa 
  toEnum 4 = Sol
  toEnum 5 = La 
  toEnum 6 = Si 
--   toEnum n
--     | (n < 0) = DDown (toEnum (n + 7))
--     | otherwise = DUp (toEnum (n - 7))


instance Enum Skip where
  fromEnum Fir = 0
  fromEnum Sec = 1
  fromEnum Thi = 2
  fromEnum Fou = 3
  fromEnum Fif = 4
  fromEnum Six = 5
  fromEnum Sev = 6
--   fromEnum (Com s) = 7 + (fromEnum s)
--   fromEnum (Neg s) = -1 * (fromEnum s)

  toEnum n
--     | (n < 0) = Neg (toEnum (-1 * n))
--     | (n == 7) = Com Fir
--     | (n > 7) = Com (toEnum (n - 7))
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



type Note1 = AbstractNote AbstractPitch1 AbstractInt1 AbstractDur1
type Note2 = AbstractNote AbstractPitch2 AbstractInt2 AbstractDur2
type Note3 = AbstractNote AbstractPitch3 AbstractInt3 AbstractDur3

instance Show (AbstractNote p i d) where
  show (AbstractPitch p d) = "Note{" ++ (show p) ++ " " ++ (showDur d) ++ "}"
  show (AbstractInt i d) = "Interval{" ++ (show i) ++ " " ++ (showDur d) ++ "}"
  show (Rest d) = "Rest{" ++ (showRest d) ++ "}"
  show (Conn c) = "Conn{" ++ (show c) ++ "}"
--  show (ConnInt i c) = "{" ++ (show i) ++ "|" ++ (show c) ++ "}"
  show (Dir c) = "{" ++ (show c) ++ "}"

-- deriving instance Show (AbstractNote p i d)

-- instance Show Note2 where
  -- show (AbstractPitch p d) = "{" ++ (show p) ++ " " ++ (showDur d) ++ "}"
  -- show (AbstractInt i d) = "{" ++ (show i) ++ " " ++ (showDur d) ++ "}"
  -- show (Rest d) = "{" ++ (showRest d) ++ "}"
  -- show (Conn c) = "{" ++ (show c) ++ "}"
-- --  show (ConnInt i c) = "{" ++ (show i) ++ "|" ++ (show c) ++ "}"
  -- show (Dir c) = "{" ++ (show c) ++ "}"

------------------------------------
-- Instances of classes defined above.

instance (Transpose p i, Duration d) => Note p i d where

-- instance (Transpose p i, Pitch p, Interval i, Duration d) => Note p i d where

-- instance Note Figuring AbstractInt1 AbstractDur2 where

addFicta Raise Raise = Raise
addFicta Raise Lower = Neutral
addFicta Lower Raise = Neutral
addFicta Lower Lower = Lower
addFicta Neutral f = f
addFicta f Neutral = f

instance Pitch AbstractPitch1 where
  sharpen (AbstractPitch1 d f) = AbstractPitch1 d (addFicta Raise f)
  flatten (AbstractPitch1 d f) = AbstractPitch1 d (addFicta Lower f)
  incr (AbstractPitch1 d f) = AbstractPitch1 (succ d) Neutral
  decr (AbstractPitch1 d f) = AbstractPitch1 (pred d) Neutral
  middle = AbstractPitch1 Ut Neutral

instance Pitch AbstractPitch2 where
  sharpen (AbstractPitch2 n a) = toPitch $ (faPitch n a) + (1 ::+ 0)
  flatten (AbstractPitch2 n a) = toPitch $ (faPitch n a) - (1 ::+ 0)
  incr (AbstractPitch2 n a) = AbstractPitch2 (succ n) a
  decr (AbstractPitch2 n a) = AbstractPitch2 (pred n) a
  middle = AbstractPitch2 A Na

instance Eq AbstractPitch2 where
  (==) = (==) `under` pitchToFa


cent :: FreqRat
cent = (2 ** (1/1200))

instance Pitch AbstractPitch3 where
  sharpen (AbstractPitch3 f) = AbstractPitch3 (f * (1 + 50*cent))
  flatten (AbstractPitch3 f) = AbstractPitch3 (f * (1 - 50*cent))
  incr (AbstractPitch3 f) = AbstractPitch3 (f * (1 + 100*cent))
  decr (AbstractPitch3 f) = AbstractPitch3 (f * (1 - 100*cent))
  middle = AbstractPitch3 440

-- instance Pitch Figuring where
--  sharpen = augment
--  flatten = diminish
--  incr = grow
--  decr = shrink


instance Interval AbstractInt1 where
  add (AbstractInt1 s f) (AbstractInt1 t g) = AbstractInt1 (toEnum $ (fromEnum s) + (fromEnum t)) Neutral
  sub (AbstractInt1 s f) (AbstractInt1 t g) = AbstractInt1 (toEnum $ (fromEnum s) - (fromEnum t)) Neutral
  grow (AbstractInt1 s _) = AbstractInt1 ((toEnum . (+ 1) . fromEnum) s) Neutral
  shrink (AbstractInt1 s _) = AbstractInt1 ((toEnum . (+(-1)) . fromEnum) s) Neutral
  augment (AbstractInt1 s f) = AbstractInt1 s (addFicta Raise f)
  diminish (AbstractInt1 s f) = AbstractInt1 s (addFicta Lower f)
  unison = AbstractInt1 Fir Neutral
  octave = AbstractInt1 (Com Fir) Neutral


instance (Interval i) => AdditiveGroup i where
  zeroV = unison
  (^+^) = add
  negateV = negate


-- not possible, no overlapping associated types allowed in GHC yet :-/
-- instance Interval i => VectorSpace i where 
--   type Scalar i = Int
--   (*^) 0 i = zeroV
--   (*^) s i
--      | (s > 0) = i ^+^ ((s - 1) *^ i)
--      | (s < 0) = (negateV i) ^+^ ((s + 1) *^ i)

intervalPowerPositive 0 i = zeroV
intervalPowerPositive s i
  | (s > 0) = i ^+^ ((s - 1) *^ i)
  | (s < 0) = (negateV i) ^+^ ((s + 1) *^ i)

instance VectorSpace AbstractInt1 where
  type Scalar AbstractInt1 = Int
  (*^) = intervalPowerPositive

instance VectorSpace AbstractInt2 where
  type Scalar AbstractInt2 = Int
  (*^) = intervalPowerPositive

instance VectorSpace AbstractInt3 where
  type Scalar AbstractInt3 = Double
  (*^) s (AbstractInt3 f) = AbstractInt3 $ f ** s

-- instance (Pitch p) => AffineSpace p where  -- not possible
--   type (Diff p) = (Transpose p i) => i
--   (.-.) = interval
--   (.+^) = flip transpose

instance AffineSpace AbstractPitch1 where
  type Diff AbstractPitch1 = AbstractInt1
  (.-.) = flip interval
  (.+^) = flip transpose

instance AffineSpace AbstractPitch2 where
  type Diff AbstractPitch2 = AbstractInt2
  (.-.) = flip interval
  (.+^) = flip transpose

instance AffineSpace AbstractPitch3 where
  type Diff AbstractPitch3 = AbstractInt3
  (.-.) = flip interval
  (.+^) = flip transpose

data FreeAbelian = Int ::+ Int deriving (Show, Eq)
-- We're using the type 'FreeAbelian' to represent (n * A1, m * d2).
--
-- Essentially, intervals form a free Abelian group G = {(n*e_1,m*e_2)
-- | (n,m) ∊ ℤ×ℤ} where e_1 = (1,0) and e_2 = (0,1) are two possible
-- elements that can be used as a basis (generators for the group.
-- An interval ratio is then defined as (A1)^n * (d2)^m.
--
-- Note that, in Pythagorean tuning, d2 is comma^(-1) (in negative
-- direction), and m2 + comma = A1 (i.e. m2 < A1).
--
-- Could use P8 & P5 as generators, or m2 & d2, or any pair of
-- linearly independent intervals (linear independence is preserved
-- across basis changes).

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
  abs (a ::+ b) = (abs a) ::+ (abs b) -- (cannot give the absolute magnitude of a group element until we know what tuning system we're using)
  signum (a ::+ b) = (signum a) ::+ (signum b)

faInt :: Quality -> Number -> FreeAbelian
-- i.e. intervals as elements of the free Abelian group

faInt Perf Unison = 0 ::+ 0
faInt (Aug Perf) Unison = 1 ::+ 0
faInt (Dim Min) Second = 0 ::+ 1

faInt Perf (Compound Unison) = (faInt Maj Seventh) + (faInt Min Second)

faInt q n@(Negative _) = faInt' q n
faInt q n@(Compound _) = faInt' q n

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

faInt (Dim q) n = (faInt q n) - (faInt (Aug Perf) Unison)
faInt (Aug q) n = (faInt q n) + (faInt (Aug Perf) Unison)

faInt' q n = let comps = countComp n
                 negs = countNeg n
                 i = faInt q (justNum n)
             in (i + ((comps ::+ 0) * (faInt Perf (Compound Unison)))) * (negs ::+ 0)

countComp (Compound n) = (countComp n) + 1
countComp (Negative n) = countComp n
countComp _ = 0

countNeg (Negative n) = (countNeg n) * (-1)
countNeg (Compound n) = countNeg n
countNeg _ = 1

justNum (Negative n) = justNum n
justNum (Compound n) = justNum n
justNum n = n

faIntNorm (n ::+ m)
  | (n <= 0) && (m <= 0) = faIntNorm ((-n) ::+ (-m))
  | otherwise = (n - (12 * (oct m))) ::+ (m `mod` 7)

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
-- note: these last two cases *have* to be this way round, otherwise
-- infinite loop occurs.
  | (n > 12) || (m > 7) = faIntToQual ((n - 12) ::+ (m - 7))
  | (n < 0) || (m < 0) = faIntToQual ((-n) ::+ (-m))


instance Interval AbstractInt2 where
  add (AbstractInt2 q n) (AbstractInt2 p m) = toInterval $ (faInt q n) + (faInt p m)
  sub (AbstractInt2 q n) (AbstractInt2 p m) = toInterval $ (faInt q n) - (faInt p m)
  augment (AbstractInt2 q n) = toInterval $ (faInt q n) + (1 ::+ 0)
  diminish (AbstractInt2 q n) = toInterval $ (faInt q n) - (1 ::+ 0)
  grow (AbstractInt2 q n) = toInterval $ (faInt q n) + (1 ::+ 1)
  shrink (AbstractInt2 q n) = toInterval $ (faInt q n) - (1 ::+ 1)
  octave = AbstractInt2 Perf (Compound Unison)
  unison = AbstractInt2 Perf Unison

instance Eq AbstractInt2 where
  (==) = (==) `under` intToFa


instance Interval AbstractInt3 where
  add (AbstractInt3 f) (AbstractInt3 g) = AbstractInt3 (f*g)
  sub (AbstractInt3 f) (AbstractInt3 g) = AbstractInt3 (f/g)
  augment (AbstractInt3 f) = AbstractInt3 (f * (1 + 100*cent))
  diminish (AbstractInt3 f) = AbstractInt3 (f * (1 - 100*cent))
  grow (AbstractInt3 f) = AbstractInt3 (f * (1 + 200*cent))
  shrink (AbstractInt3 f) = AbstractInt3 (f * (1 - 200*cent))
  unison = AbstractInt3 1
  octave = AbstractInt3 2

instance Transpose AbstractPitch1 AbstractInt1 where
  transpose (AbstractInt1 s f') (AbstractPitch1 d f) = AbstractPitch1 (toEnum $ (fromEnum s) + (fromEnum d)) (addFicta f' f)
  interval (AbstractPitch1 d _) (AbstractPitch1 d' _) = AbstractInt1 (toEnum $ (fromEnum d') - (fromEnum d)) Neutral
  normalise (AbstractPitch1 d f) (AbstractInt1 s _) (AbstractPitch1 e g)
    | s < (Com Fir) = undefined
    | (current >= lower) && (current < upper) = AbstractPitch1 d f
    | current < lower = normalise (AbstractPitch1 d f) (AbstractInt1 s Neutral) (transpose (AbstractInt1 (Com Fir) Neutral) (AbstractPitch1 e g))
    | otherwise = normalise (AbstractPitch1 d f) (AbstractInt1 s Neutral) (transpose (AbstractInt1 (Neg (Com Fir)) Neutral) (AbstractPitch1 e g))
    where lower = fromEnum d
          upper = lower + (fromEnum s)
          current = fromEnum e

-- instance Transpose Figuring AbstractInt1 where
--   transpose = (^+^)
--   interval = (^-^)


faPitch :: Name -> Accidental -> FreeAbelian

-- Pitches as elements of the rank-2 free Abelian group -- however,
-- this is only as an implementation detail: all pitches are
-- explicitly measured as intervals relative to middle-A-natural. To
-- the user, pitches still form an affine space.

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
-- toPitch (n ::+ m) = AbstractPitch2 (toEnum m) (faToAcc (n ::+ m))
toPitch (n ::+ m) = AbstractPitch2 (toEnum m) ((faToAcc . faNorm) (n ::+ m))

pitchToFa (AbstractPitch2 n a) = faPitch n a

oct m = m `div` 7

faNorm (n ::+ m) = (n - (12 * (oct m))) ::+ (m `mod` 7)

-- Only for pitches the lie within the span of one octave above
-- middle-A (e.g. that have been normalised by faNorm).
faToAcc (n ::+ m)
  | (n < 0) && (m == 0) = Fl (faToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (0 ::+ 0) = Na
  | (n > 0) && (m == 0) = Sh (faToAcc ((n - 1) ::+ m))
  | (n < 2) && (m == 1) = Fl (faToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (2 ::+ 1) = Na
  | (n > 2) && (m == 1) = Sh (faToAcc ((n - 1) ::+ m))
  | (n < 3) && (m == 2) = Fl (faToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (3 ::+ 2) = Na
  | (n > 3) && (m == 2) = Sh (faToAcc ((n - 1) ::+ m))
  | (n < 5) && (m == 3) = Fl (faToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (5 ::+ 3) = Na
  | (n > 5) && (m == 3) = Sh (faToAcc ((n - 1) ::+ m))
  | (n < 7) && (m == 4) = Fl (faToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (7 ::+ 4) = Na
  | (n > 7) && (m == 4) = Sh (faToAcc ((n - 1) ::+ m))
  | (n < 8) && (m == 5) = Fl (faToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (8 ::+ 5) = Na
  | (n > 8) && (m == 5) = Sh (faToAcc ((n - 1) ::+ m))
  | (n < 10) && (m == 6) = Fl (faToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (10 ::+ 6) = Na
  | (n > 10) && (m == 6) = Sh (faToAcc ((n - 1) ::+ m))
  | (n < 12) && (m == 7) = Fl (faToAcc ((n + 1) ::+ m))
  | (n ::+ m) == (12 ::+ 7) = Na
  | (n > 12) && (m == 7) = Sh (faToAcc ((n - 1) ::+ m))


instance Transpose AbstractPitch2 AbstractInt2 where
  transpose (AbstractInt2 q i) (AbstractPitch2 n a) = toPitch $ (faPitch n a) + (faInt q i)
  interval (AbstractPitch2 n a) (AbstractPitch2 p b) = toInterval $ (faPitch p b) - (faPitch n a)
  normalise base diff n
    | diff < (AbstractInt2 Maj Seventh) = undefined
    | (n >= base) && (n < upper) = n
    | n < base = normalise base diff (n .+^ octave)
    | otherwise = normalise base diff (n .-^ octave)
    where upper = base .+^ diff


instance Transpose AbstractPitch3 AbstractInt3 where
  transpose (AbstractInt3 i) (AbstractPitch3 f) = AbstractPitch3 (f * i)
  interval (AbstractPitch3 f) (AbstractPitch3 g) = AbstractInt3 (g / f)
  normalise (AbstractPitch3 f) (AbstractInt3 i) (AbstractPitch3 g)
    | upper/lower <= 0.5 = undefined
    | (current >= lower) && (current < upper) = AbstractPitch3 g
    | current < lower = normalise (AbstractPitch3 f) (AbstractInt3 i) (AbstractPitch3 (g*2))
    | otherwise = normalise (AbstractPitch3 f) (AbstractInt3 i) (AbstractPitch3 (g/2))
    where lower = f
          upper = f * i
          current = g


instance Duration AbstractDur1 where
  unit = AbstractDur1 Br
  zeroD = error "zero-length mensuration duration not implemented yet"
  combine (AbstractDur1 a) (AbstractDur1 b) = AbstractDur1 $ MTie a b
  subD = error "cannot subtract mensurations yet"

instance Duration AbstractDur2 where
  unit = AbstractDur2 (1 % 1)
  zeroD = AbstractDur2 (0 % 1)
  -- combine (AbstractDur2 (nd -> (0, _))) _ = error "no zero durations!"
  -- combine _ (AbstractDur2 (nd -> (0, _))) = error "no zero durations!"
  combine (AbstractDur2 r) (AbstractDur2 s) = AbstractDur2 (r + s)
  subD (AbstractDur2 r) (AbstractDur2 s) = AbstractDur2 (r - s)

  showDur (AbstractDur2 (nd -> (2, 1))) = "𝅜"
  showDur (AbstractDur2 (nd -> (1, 1))) = "𝅝"
  showDur (AbstractDur2 (nd -> (1, 2))) = "𝅗𝅥"
  showDur (AbstractDur2 (nd -> (1, 4))) = "𝅘𝅥"
  showDur (AbstractDur2 (nd -> (1, 8))) = "𝅘𝅥𝅮"
  showDur (AbstractDur2 (nd -> (1, 16))) = "𝅘𝅥𝅯"
  showDur (AbstractDur2 (nd -> (1, 32))) = "𝅘𝅥𝅰"
  showDur (AbstractDur2 (nd -> (1, 64))) = "𝅘𝅥𝅱"
  showDur (AbstractDur2 (nd -> (1, 128))) = "𝅘𝅥𝅲"
  showDur (AbstractDur2 r) = show r

  showRest (AbstractDur2 (nd -> (2, 1))) = "𝄺"
  showRest (AbstractDur2 (nd -> (1, 1))) = "𝄻"
  showRest (AbstractDur2 (nd -> (1, 2))) = "𝄼"
  showRest (AbstractDur2 (nd -> (1, 4))) = "𝄽"
  showRest (AbstractDur2 (nd -> (1, 8))) = "𝄾"
  showRest (AbstractDur2 (nd -> (1, 16))) = "𝄿"
  showRest (AbstractDur2 (nd -> (1, 32))) = "𝅀"
  showRest (AbstractDur2 (nd -> (1, 64))) = "𝅁"
  showRest (AbstractDur2 (nd -> (1, 128))) = "𝅂"
  showRest (AbstractDur2 r) = show r


instance Duration AbstractDur3 where
  zeroD = AbstractDur3 0
  unit = AbstractDur3 1
  -- combine (AbstractDur3 0) _ = error "no zero durations!"
  -- combine _ (AbstractDur3 0) = error "no zero durations!"
  combine (AbstractDur3 t) (AbstractDur3 r) = AbstractDur3 (t + r)
  subD (AbstractDur3 t) (AbstractDur3 r) = AbstractDur3 (t - r)


-- Durations are a semigroup because zero-length durations are
-- forbidden.
instance (Duration d) => Semigroup d where
  (<>) = combine

instance Timing Metronome AbstractDur2 where
  time (Metronome n) (AbstractDur2 r) = AbstractDur3 (realToFrac ((240000 % n) * r))
  -- time (Metronome n) (AbstractDur2 r) = AbstractDur3 (240000/(fromIntegral n) * (fromRational r))
-- todo: check this calculation
-- 60 bpm = 15 sbpm
-- 1 sb = 60s/15 = 60s/(bpm/4) = 60000 ms / (bpm/4)


-- An example function to use with mapPhrase
sharpenAndDouble :: Note p i d => AbstractNote p i d -> AbstractNote p i d
sharpenAndDouble = (apPitch sharpen) . (apInt augment) . (apDur (\d -> combine d d))

mapPhrase :: (Note p i d, Note p' i' d')
             => (AbstractNote p i d -> AbstractNote p' i' d')
             -> AbstractPhrase (AbstractNote p i d) -> AbstractPhrase (AbstractNote p' i' d')
mapPhrase f (AbstractPhrase ((Conn p):[])) = AbstractPhrase [Conn (mapPhrase f p)]
mapPhrase f (AbstractPhrase (n:[])) = AbstractPhrase [f n]
mapPhrase f (AbstractPhrase ((Conn p):ns)) = (AbstractPhrase [Conn (mapPhrase f p)]) <> (mapPhrase f (AbstractPhrase ns))
mapPhrase f (AbstractPhrase (n:ns)) = (AbstractPhrase [f n]) <> (mapPhrase f (AbstractPhrase ns))

-- mapPhrase without recursion into sub-phrases
mapPhraseSingle :: (Note p i d)
                   => (AbstractNote p i d -> AbstractNote p i d)
                   -> AbstractPhrase (AbstractNote p i d) -> AbstractPhrase (AbstractNote p i d)
mapPhraseSingle f (AbstractPhrase ((Conn p):[])) = AbstractPhrase [Conn (mapPhraseSingle f p)]
mapPhraseSingle f (AbstractPhrase (n:[])) = AbstractPhrase [f n]
mapPhraseSingle f (AbstractPhrase ((Conn p):ns)) = (AbstractPhrase [Conn p]) <> (mapPhraseSingle f (AbstractPhrase ns))
mapPhraseSingle f (AbstractPhrase (n:ns)) = (AbstractPhrase [f n]) <> (mapPhraseSingle f (AbstractPhrase ns))



-- by analogy with fold1
foldPhrase1 :: Note p i d
              => (AbstractNote p i d -> AbstractNote p i d -> AbstractNote p i d)
              -> AbstractPhrase (AbstractNote p i d) -> AbstractNote p i d

foldPhrase1 f (AbstractPhrase (n:[])) =
  case n of (Conn p) -> foldPhrase1 f p
            p -> p

foldPhrase1 f (AbstractPhrase (n:ns)) =
  case n of (Conn p) -> f (foldPhrase1 f p) (foldPhrase1 f (AbstractPhrase ns))
            (Dir _) -> foldPhrase1 f (AbstractPhrase ns)
            p -> f p (foldPhrase1 f (AbstractPhrase ns))

foldPhrase1 _ _ = error "Exhausted patterns in foldPhrase1"

foldPhrase :: Note p i d
              => (AbstractNote p i d -> a -> a)
              -> a -> AbstractPhrase (AbstractNote p i d) -> a

foldPhrase f e (AbstractPhrase (n:[])) =
  case n of (Conn p) -> foldPhrase f e p
            p -> f p e

foldPhrase f e (AbstractPhrase (n:ns)) =
  case n of (Conn p) -> let branch1 = foldPhrase f e (AbstractPhrase ns)
                            branch2 = foldPhrase f branch1 p
                        in branch2
            (Dir _) -> foldPhrase f e (AbstractPhrase ns)
            p -> f p (foldPhrase f e (AbstractPhrase ns))


flattenPhrase :: (AbstractPhrase t) -> (AbstractPhrase t)
flattenPhrase p@(AbstractPhrase (_:[])) = p
flattenPhrase (AbstractPhrase (n:ns)) =
  case n of (Conn p) -> p <> (flattenPhrase (AbstractPhrase ns))
            q -> (AbstractPhrase [q]) <> (flattenPhrase (AbstractPhrase ns))
flattenPhrase (AbstractPhrase []) = AbstractPhrase []

-- foldPhrase1 with *no* recursion into connected phrases -- they're simply ignored.
foldPhraseSingle :: Note p i d
                    => (AbstractNote p i d -> AbstractNote p i d -> AbstractNote p i d)
                    -> AbstractPhrase (AbstractNote p i d) -> AbstractNote p i d

foldPhraseSingle f (AbstractPhrase p) = foldPhrase' f (AbstractPhrase (filter (not . isConn) p)) where
  foldPhrase' :: Note p i d
                 => (AbstractNote p i d -> AbstractNote p i d -> AbstractNote p i d)
                 -> AbstractPhrase (AbstractNote p i d) -> AbstractNote p i d

  foldPhrase' f (AbstractPhrase (n:(Conn _):[])) = n
  foldPhrase' f (AbstractPhrase (n:(Dir _):[])) = n
  foldPhrase' f (AbstractPhrase (n:[])) = n
  foldPhrase' f (AbstractPhrase ((Conn _):ns)) = foldPhrase' f (AbstractPhrase ns)
  foldPhrase' f (AbstractPhrase ((Dir _):ns)) = foldPhrase' f (AbstractPhrase ns)
  foldPhrase' f (AbstractPhrase (n:(Conn _):ns)) = f n (foldPhrase' f (AbstractPhrase ns))
  foldPhrase' f (AbstractPhrase (n:(Dir _):ns)) = f n (foldPhrase' f (AbstractPhrase ns))
  foldPhrase' f (AbstractPhrase (n:ns)) = f n (foldPhrase' f (AbstractPhrase ns))
  foldPhrase' _ p = error ("Exhausted patterns in foldPhraseSingle: " ++ (show p))

-- extractDur :: (Note p i d) => (AbstractNote p i d) -> d
extractDur (AbstractPitch _ d) = d
extractDur (AbstractInt _ d) = d
extractDur (Rest d) = d
extractDur _ = zeroD
-- extractDur p = error ("Trying to extract duration from value with no duration: " ++ (show p))

-- extractPitch :: (Note p i d) => (AbstractNote p i d) -> p
extractPitch (AbstractPitch p _) = p
extractPitch p = error ("Trying to extract duration from value with no duration: " ++ (show p))


countDurs :: Note p i d => AbstractPhrase (AbstractNote p i d) -> d
countDurs (AbstractPhrase []) = zeroD
countDurs (AbstractPhrase p) = extractDur (countDurs' (AbstractPhrase (filter isNote p))) where
  countDurs' (AbstractPhrase []) = Rest zeroD
  countDurs' p = foldPhraseSingle (\n n' -> Rest $ (extractDur n) <> (extractDur n')) p

countDursRec :: Note p i d => AbstractPhrase (AbstractNote p i d) -> d
countDursRec (AbstractPhrase []) = zeroD
countDursRec (AbstractPhrase p) = extractDur (countDurs' (AbstractPhrase (filter isNote p))) where
  countDurs' (AbstractPhrase []) = Rest zeroD
  countDurs' p = foldPhrase1 (\n n' -> Rest $ (extractDur n) <> (extractDur n')) p

splitVoices :: (Note p i d) =>
               AbstractPhrase (AbstractNote p i d) -> [AbstractPhrase (AbstractNote p i d)]
-- Walk along phrase until we find a connection to another phrase;
-- split off and insert correct number of rests in front of new
-- phrase. Repeat this procedure on all newly-discovered phrases,
-- recursively.
splitVoices (AbstractPhrase ns) =
  let before = takeWhile (not . isConn) ns
      after' = dropWhile (not . isConn) ns
      connector = head after'
      after = tail after'
      rest = AbstractPhrase $ [Rest $ countDurs (AbstractPhrase before)]
  in if null after'
     then [AbstractPhrase before]
     else case connector of
       (Conn p) -> (splitVoices (AbstractPhrase (before ++ after))) ++ (splitVoices (if null before
                                                                                     then p
                                                                                     else (rest <> p)))





firstPitch :: [AbstractNote p i d] -> Maybe (AbstractNote p i d)
firstPitch ((AbstractPitch p d):ns) = Just $ AbstractPitch p d
firstPitch ((AbstractInt _ _):ns) = Nothing -- error "Cannot make phrase absolute if it has relative pitches before absolute pitches."
firstPitch [] = Nothing
firstPitch (_:ns) = firstPitch ns


-- todo: rewrite absolute in a foldPhrase/countDurs style.
absolute :: (Note p i d) =>
            AbstractPhrase (AbstractNote p i d) -> AbstractPhrase (AbstractNote p i d)
absolute (AbstractPhrase ns) = AbstractPhrase (absolute' n ns)
  where n = case firstPitch ns of
             Just n' -> n'
             Nothing -> error "arrrgh"
        absolute' _ ((AbstractPitch p d):notes) = (AbstractPitch p d) : (absolute' (AbstractPitch p d) notes)
        absolute' base@(AbstractPitch p _) ((AbstractInt i d):notes) = (AbstractPitch (transpose i p) d) : (absolute' (AbstractPitch (transpose i p) d) notes)
        absolute' base ((Conn (AbstractPhrase p)):notes) = let remaining = absolute' base notes
                                                               base' = case firstPitch remaining of
                                                                        Just n' -> n'
                                                                        Nothing -> error "arrrgh"
                                                               connected = (Conn (AbstractPhrase (absolute' base' p)))
                                                           in connected : remaining
        absolute' base (p:notes) = p : (absolute' base notes)
        absolute' _ [] = []


-- Apply a function just to absolute pitches
apPitch f (AbstractPitch p d) = AbstractPitch (f p) d
apPitch _ p = p

-- Apply a function just to relative pitches
apInt f (AbstractInt p d) = AbstractInt (f p) d
apInt _ p = p

-- Apply a function just to rests
apRest f (Rest d) = Rest (f d)
apRest _ p = p

-- Transpose pitches, ignore everything else
apTran i (AbstractPitch p d) = AbstractPitch (p .+^ i) d
apTran i (AbstractInt p d) = AbstractInt (p ^+^ i) d
apTran _ p = p

-- Manipulate durations, ignore everything else
apDur f (AbstractPitch p d) = AbstractPitch p (f d)
apDur f (AbstractInt p d) = AbstractInt p (f d)
apDur f (Rest d) = Rest (f d)
apDur _ p = p



--------

instance Show (AbstractNote p i d) => Show (AbstractPhrase (AbstractNote p i d)) where
  show (AbstractPhrase x) = show x

-- -- Phrases are a semigroup because phrases containing zero notes are forbidden.
-- instance (Note p i d) => Semigroup (AbstractPhrase (AbstractNote p i d)) where
  -- (AbstractPhrase []) <> _ = error "no empty phrases!"
  -- _ <> (AbstractPhrase []) = error "no empty phrases!"
  -- (AbstractPhrase a) <> (AbstractPhrase b) = AbstractPhrase $ a ++ b

-- Relaxing this condition for now, as PhraseContext needs to be able
-- to manipulate zero-length phrases.
instance (Note p i d) => Semigroup (AbstractPhrase (AbstractNote p i d)) where
  (AbstractPhrase []) <> p = p
  p <> (AbstractPhrase []) = p
  (AbstractPhrase a) <> (AbstractPhrase b) = AbstractPhrase $ a ++ b

  
emptyPhrase :: (Note p i d) => AbstractPhrase (AbstractNote p i d)
emptyPhrase = AbstractPhrase []

repeatPhrase 0 _ = error "no empty phrases!" -- hmmmm
repeatPhrase 1 p = p
repeatPhrase n p = p <> (repeatPhrase (n - 1) p)


explodeVoices :: (Note p i d) => Music (AbstractNote p i d) -> Music (AbstractNote p i d)
explodeVoices (Start p) = Voices $ filter (/= emptyPhrase) $ splitVoices p
explodeVoices (Voices ps) = Voices $ concatMap splitVoices ps

revVoices (Voices ps) = Voices $ reverse ps
revVoices m = m

-- Apply a tuning system and a timing to a note, simultaneously. Use
-- with mapPhrase to turn AbstractNote2s into AbstractNote3s.
----noteToSound :: (Note p i d, Tuning t p i, Timing t' d) => t -> t' -> AbstractNote p i d -> Note3
----noteToSound tuning timing (AbstractPitch p d) = AbstractPitch (tune tuning p) (time timing d)
----noteToSound _ timing (Rest d) = Rest (time timing d)
----noteToSound _ timing (Dir d) = (Dir d)
noteToSound :: (Note p i d, Tuning t p i, Timing t' d) => t -> t' -> AbstractNote p i d -> Note3
noteToSound tu ti = (timeNote ti) . (tuneNote tu)


mapMusic :: (Note p i d, Note p' i' d')
             => (AbstractPhrase (AbstractNote p i d) ->
                 AbstractPhrase (AbstractNote p' i' d')) ->
            Music (AbstractNote p i d) -> Music (AbstractNote p' i' d')
-- Apply a function to all phrases in a piece of music.
mapMusic f (Start p) = Start $ f p
mapMusic f (Voices ps) = Voices $ map f ps


instance (Note p i d) => Semigroup (Music (AbstractNote p i d)) where
  (Voices v) <> (Voices v') = Voices $ uniq (v ++ v')
  (Voices v) <> (Start p) = (Voices v) <> (explodeVoices (Start p))
  (Start p) <> (Voices v) = (explodeVoices (Start p)) <> (Voices v)
  (Start p) <> (Start p') = Start $ (phrase [(Conn p')]) <> p



-- Function to produce a chord
chord :: (Note p i d) => d -> [p] -> AbstractPhrase (AbstractNote p i d)
chord _ [] = error "no empty phrases!"
chord d (p:[]) = phrase [note p d]
chord d (p:ps) = let r = chord d ps
                 in phrase [Conn r, note p d]


-- remove duration from note
dropDur :: (Note p i d) => d -> (AbstractNote p i d) -> (AbstractNote p i d)
dropDur d n = let d' = extractDur n
              in if d' < d
                 then apDur (\_ -> zeroD) n
                 else apDur (\_ -> subD d' d) n

-- remove a certain duration of notes from beginning of phrase
dropPhrase :: (Note p i d) => d -> AbstractPhrase (AbstractNote p i d) -> AbstractPhrase (AbstractNote p i d)
dropPhrase d p@(AbstractPhrase (x:xs))
  | d == zeroD = p
  | d >= extractDur x = dropPhrase (subD d (extractDur x)) (AbstractPhrase xs)
  | otherwise = AbstractPhrase ((dropDur d x):xs)
dropPhrase _ (AbstractPhrase []) = AbstractPhrase []

reversePhrase :: (Note p i d) => AbstractPhrase (AbstractNote p i d) -> AbstractPhrase (AbstractNote p i d)
reversePhrase (AbstractPhrase p) = AbstractPhrase (reverse p)

dropPhraseEnd :: (Note p i d) => d -> AbstractPhrase (AbstractNote p i d) -> AbstractPhrase (AbstractNote p i d)
dropPhraseEnd d = reversePhrase . (dropPhrase d) . reversePhrase
