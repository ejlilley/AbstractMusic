{-# LANGUAGE MultiParamTypeClasses, 
             UndecidableInstances, 
             FunctionalDependencies,
             FlexibleContexts,
             OverlappingInstances,
             TypeSynonymInstances,
             ScopedTypeVariables,
             UnicodeSyntax,
             GADTSyntax,
             GADTs,
             TypeFamilies,
             FlexibleInstances #-}

module FiveLimit where

import Prelude hiding (negate)

import Music (Number(..),
              Tuning(..), AbstractPitch2(..), AbstractPitch3(..),
              AbstractInt2(..), AbstractInt3(..), Scale(..),
              AbstractDur2(..), Note(..),
              Interval(..), Transpose(..), Pitch(..),
              Name(..), Accidental(..),
              countComp, countNeg, justNum, Music(..),
              mapMusic, mapPhrase, noteToSound, Metronome(..))

import Util (uniq, under)

import Shortcuts (m, cr, q, a, b, c, d, e, f, g)

import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace

import Scales (major, minor, infiniteScale, chromaticScale)

justTune :: AbstractInt3 -> AbstractInt3 -> AbstractInt3 -> JustInt -> AbstractInt3
justTune ac1rat a1rat d2rat (JustInt qu nu) =
  let FA3 (m,n,p) = faIntJ qu nu
  in (ac1rat ^* (fromIntegral m)) ^+^ (a1rat ^* (fromIntegral n)) ^+^ (d2rat ^* (fromIntegral p))


data FreeAbelian3 = FA3 (Int, Int, Int) deriving (Eq)
-- Rank 3 free Abelian group, (L1,G1,d2) ∊ ℤ×ℤ×ℤ, where L1 = L2 - m2,
-- G1 = G2 - L2, d2 = m2 - L1 - G1. (m2 is a semitone, L2 is a minor
-- tone, G2 is a major tone).

instance Num FreeAbelian3 where
  (FA3 (a,b,c)) + (FA3 (d,e,f)) = FA3 (a+d,b+e,c+f)
  (FA3 (a,b,c)) - (FA3 (d,e,f)) = FA3 (a-d,b-e,c-f)
  (FA3 (a,b,c)) * (FA3 (d,e,f)) = FA3 (a*d,b*e,c*f)
  fromInteger _ = undefined
  abs _ = undefined
  signum _ = undefined

instance Show FreeAbelian3 where
  show (FA3 (m,n,p)) = "<" ++ (show m) ++ "," ++ (show n) ++ "," ++ (show p) ++ ">"


type Key = AbstractPitch2

data JustPitch = JustPitch Key AbstractPitch2

instance Eq JustPitch where
  (==) = (==) `under` jpToFa

instance Show JustPitch where
  show (JustPitch k p) = "[" ++ (show p) ++ " in " ++ (show k) ++ "]"

data JustInt = JustInt JustQuality Number

instance Eq JustInt where
  (==) = (==) `under` intJtoFa

instance Show JustInt where
  show (JustInt q n) = (show q) ++ (show n)

instance Ord JustInt where
  (JustInt _ n) `compare` (JustInt _ m) = (fromEnum n) `compare` (fromEnum m)

instance Ord JustPitch where
  (JustPitch _ (AbstractPitch2 n _)) `compare` (JustPitch _ (AbstractPitch2 m _)) =
    (fromEnum n) `compare` (fromEnum m)


jp = JustPitch

data JustQuality = Perf | Min | Maj | Lesser | Greater
                 | Acute JustQuality
                 | Grave JustQuality
                 | Dim JustQuality
                 | Aug JustQuality
                 deriving (Eq)

instance Show JustQuality where
  show Perf = "P"
  show Maj = "M"
  show Min = "m"
  show Lesser = "L"
  show Greater = "G"
--   show (Dim Perf) = "d"
--   show (Dim Maj) = "d"
--   show (Dim Min) = "d"
--   show (Dim Lesser) = "d"
--   show (Dim Greater) = "d"
--   show (Aug Perf) = "A"
--   show (Aug Maj) = "A"
--   show (Aug Min) = "A"
--   show (Aug Greater) = "A"
--   show (Aug Lesser) = "A"
  show (Aug q) = 'A':(show q)
  show (Dim q) = 'd':(show q)
  show (Acute q) = "Ac" ++ (show q)
  show (Grave q) = "Gr" ++ (show q)


-- Note that (Key, Pitch, Accidental) aren't *completely* orthogonal,
-- and they might not determine the FA3 for that pitch uniquely.

instance AdditiveGroup JustInt where
  zeroV = unison
  (^+^) = add
  negateV = negate

instance AffineSpace JustPitch where
  type Diff JustPitch = JustInt
  (.-.) = flip interval
  (.+^) = flip transpose

instance Transpose JustPitch JustInt where
  transpose (JustInt q i) (JustPitch k p) = toJP k $ (faJP k p) + (faIntJ q i)
  interval (JustPitch k p) (JustPitch k' p') = toIntJ $ (faJP k' p') - (faJP k p)
  normalise base diff n
    | diff < (JustInt Maj Seventh) = undefined
    | (n >= base) && (n < upper) = n
    | n < base = normalise base diff (n .+^ octave)
    | otherwise = normalise base diff (n .-^ octave)
    where upper = base .+^ diff

instance VectorSpace JustInt where
  type Scalar JustInt = Int
  (*^) 0 i = zeroV
  (*^) s i
    | (s > 0) = i ^+^ ((s - 1) *^ i)
    | (s < 0) = (negateV i) ^+^ ((s + 1) *^ i)


instance Interval JustInt where
  add (JustInt q n) (JustInt p m) = toIntJ $ (faIntJ q n) + (faIntJ p m)
  sub (JustInt q n) (JustInt p m) = toIntJ $ (faIntJ q n) - (faIntJ p m)
  invert (JustInt q n) = toIntJ $ (faIntJ Perf (Compound Unison)) - (faIntJ q n)
  negate (JustInt q n) = sub (JustInt Perf Unison) (JustInt q n)
  augment (JustInt q n) = toIntJ $ (faIntJ q n) + jA1
  diminish (JustInt q n) = toIntJ $ (faIntJ q n) - jA1
  grow (JustInt q n) = toIntJ $ (faIntJ q n) + jm2
  shrink (JustInt q n) = toIntJ $ (faIntJ q n) - jm2
  octave = JustInt Perf (Compound Unison)
  unison = JustInt Perf Unison


instance Pitch JustPitch where
  sharpen (JustPitch k p) = toJP k $ (faJP k p) + jA1
  flatten (JustPitch k p) = toJP k $ (faJP k p) - jA1
  incr (JustPitch k (AbstractPitch2 n a)) = JustPitch k $ AbstractPitch2 (succ n) a
  decr (JustPitch k (AbstractPitch2 n a)) = JustPitch k $ AbstractPitch2 (pred n) a
  middle = JustPitch (AbstractPitch2 A Na) (AbstractPitch2 A Na)

jAc1 = FA3 (1,0,0) -- just comma
jA1 = FA3 (0,1,0) -- just augmented unison
jd2 = FA3 (0,0,1) -- just diminished second

jm2 = jd2 + jA1
jLM2 = jm2 + jA1
jGM2 = jLM2 + jAc1

-- natural intervals can have either:
-- Perf (1,4,5)
-- Min Lesser Greater (2)
-- Min Maj (3,6,7)

faIntJ Perf Unison = FA3 (0,0,0)

faIntJ (Aug Perf) Unison = jA1
faIntJ (Dim Min) Second = jd2

faIntJ Perf (Compound Unison) = (faIntJ Perf Fifth) + (faIntJ Perf Fourth)

faIntJ q n@(Negative _) = faIntJ' q n
faIntJ q n@(Compound _) = faIntJ' q n


faIntJ Min Second = jd2 + jA1
faIntJ Lesser Second = faIntJ (Aug Min) Second
faIntJ Greater Second = faIntJ (Acute Lesser) Second

faIntJ Min Third = (faIntJ Greater Second) + (faIntJ Min Second)
faIntJ Maj Third = (faIntJ Greater Second) + (faIntJ Lesser Second)

faIntJ Perf Fourth = (faIntJ Maj Third) + (faIntJ Min Second)

faIntJ Perf Fifth = (faIntJ Perf Fourth) + (faIntJ Greater Second)

faIntJ Min Sixth = (faIntJ Perf Fifth) + (faIntJ Min Second)
faIntJ Maj Sixth = (faIntJ Perf Fifth) + (faIntJ Lesser Second)

faIntJ Min Seventh = (faIntJ Perf Fifth) + (faIntJ Min Third)
faIntJ Maj Seventh = (faIntJ Perf Fifth) + (faIntJ Maj Third)

faIntJ (Dim q) n = (faIntJ q n) - jA1
faIntJ (Aug q) n = (faIntJ q n) + jA1
faIntJ (Grave q) n = (faIntJ q n) - jAc1
faIntJ (Acute q) n = (faIntJ q n) + jAc1

faIntJ' q n = let comps = countComp n
                  negs = countNeg n
                  i = faIntJ q (justNum n)
              in (i + (comps *^ (faIntJ Perf (Compound Unison)))) ^* negs

instance AdditiveGroup FreeAbelian3 where
  zeroV = FA3 (0,0,0)
  (^+^) = (+)
  negateV (FA3 (a,b,c)) = FA3 (-a,-b,-c)

instance VectorSpace FreeAbelian3 where
  type Scalar FreeAbelian3 = Int
  (*^) s (FA3 (m,n,p)) = FA3 (s*m,s*n,s*p)


intJtoFa (JustInt q n) = faIntJ q n

toIntJ (FA3 (m,n,p)) = JustInt (jIntToQual (m,n,p)) (toEnum p)

jIntToQual (m,n,p)
  | (p == 0) && (m < 0) = Grave (jIntToQual (m + 1,n,p))
  | (p == 0) && (n < 0) = Dim   (jIntToQual (m,n + 1,p))
  | (m,n,p) == (0,0,0) = Perf
  | (p == 0) && (m > 0) = Acute (jIntToQual (m - 1,n,p))
  | (p == 0) && (n > 0) = Aug   (jIntToQual (m,n - 1,p))
  | (p == 1) && (m < 0) = Grave (jIntToQual (m + 1,n,p))
  | (p == 1) && (n < 1) = Dim   (jIntToQual (m,n + 1,p))
  | (m,n,p) == (0,1,1) = Min
  | (m,n,p) == (0,2,1) = Lesser
  | (m,n,p) == (1,2,1) = Greater
  | (p == 1) && (m > 0) = Acute (jIntToQual (m - 1,n,p))
  | (p == 1) && (n > 1) = Aug   (jIntToQual (m,n - 1,p))
  | (p == 1) && (m < 0) = Grave (jIntToQual (m + 1,n,p))
  | (p == 1) && (n < 2) = Dim   (jIntToQual (m,n + 1,p))
  | (p == 1) && (m > 1) = Acute (jIntToQual (m - 1,n,p))
  | (p == 1) && (n > 2) = Aug   (jIntToQual (m,n - 1,p))
  | (p == 2) && (m < 1) = Grave (jIntToQual (m + 1,n,p))
  | (p == 2) && (n < 3) = Dim   (jIntToQual (m,n + 1,p))
  | (m,n,p) == (1,3,2) = Min
  | (m,n,p) == (1,4,2) = Maj
  | (p == 2) && (m > 1) = Acute (jIntToQual (m - 1,n,p))
  | (p == 2) && (n > 4) = Aug   (jIntToQual (m,n - 1,p))
  | (p == 3) && (m < 1) = Grave (jIntToQual (m + 1,n,p))
  | (p == 3) && (n < 5) = Dim   (jIntToQual (m,n + 1,p))
  | (m,n,p) == (1,5,3) = Perf
  | (p == 3) && (m > 1) = Acute (jIntToQual (m - 1,n,p))
  | (p == 3) && (n > 5) = Aug   (jIntToQual (m,n - 1,p))
  | (p == 4) && (m < 2) = Grave (jIntToQual (m + 1,n,p))
  | (p == 4) && (n < 7) = Dim   (jIntToQual (m,n + 1,p))
  | (m,n,p) == (2,7,4) = Perf
  | (p == 4) && (m > 2) = Acute (jIntToQual (m - 1,n,p))
  | (p == 4) && (n > 7) = Aug   (jIntToQual (m,n - 1,p))
  | (p == 5) && (m < 2) = Grave (jIntToQual (m + 1,n,p))
  | (p == 5) && (n < 8) = Dim   (jIntToQual (m,n + 1,p))
  | (m,n,p) == (2,8,5) = Min
  | (m,n,p) == (2,9,5) = Maj
  | (p == 5) && (m > 2) = Acute (jIntToQual (m - 1,n,p))
  | (p == 5) && (n > 9) = Aug   (jIntToQual (m,n - 1,p))
  | (p == 6) && (m < 3) = Grave (jIntToQual (m + 1,n,p))
  | (p == 6) && (n < 10) = Dim  (jIntToQual (m,n + 1,p))
  | (m,n,p) == (3,10,6) = Min
  | (m,n,p) == (3,11,6) = Maj
  | (p == 6) && (m > 3) = Acute (jIntToQual (m - 1,n,p))
  | (p == 6) && (n > 11) = Aug  (jIntToQual (m,n - 1,p))
  | (p == 7) && (m < 3) = Grave (jIntToQual (m + 1,n,p))
  | (p == 7) && (n < 12) = Dim  (jIntToQual (m,n + 1,p))
  | (m,n,p) == (3,12,7) = Perf
  | (p == 7) && (m > 3) = Acute (jIntToQual (m - 1,n,p))
  | (p == 7) && (n > 12) = Aug  (jIntToQual (m,n - 1,p))

  | (m > 3) || (n > 12) || (p > 7) = jIntToQual (m-3,n-12,p-7)
  | (m < 0) || (n < 0)  || (p < 0) = jIntToQual (-m,-n,-p)

    
faJP :: Key -> AbstractPitch2 -> FreeAbelian3 -- currently just for symmetric scale 1.

faJP k@(AbstractPitch2 C Na) (AbstractPitch2 A a) = (faJP k (AbstractPitch2 (Up A) a)) - (intJtoFa _P8)
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 B a) = (faJP k (AbstractPitch2 (Up B) a)) - (intJtoFa _P8)

faJP k@(AbstractPitch2 C Na) (AbstractPitch2 C Na)           = FA3 (0,0,0)
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 D (Fl Na))      = intJtoFa m2
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 D Na)           = intJtoFa _L2
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 E (Fl Na))      = (faJP k (AbstractPitch2 D (Fl Na))) + (intJtoFa _G2)
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 E Na)           = (faJP k (AbstractPitch2 D Na)) + (intJtoFa _G2)
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 F Na)           = (faJP k (AbstractPitch2 E Na)) + (intJtoFa m2)
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 G Na)           = (faJP k (AbstractPitch2 F Na)) + (intJtoFa _G2)
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 (Up A) (Fl Na)) = (faJP k (AbstractPitch2 G Na)) + (intJtoFa m2)
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 (Up A) Na)      = (faJP k (AbstractPitch2 G Na)) + (intJtoFa _L2)
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 (Up B) (Fl Na)) = (faJP k (AbstractPitch2 G Na)) + (intJtoFa m3)
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 (Up B) Na)      = (faJP k (AbstractPitch2 G Na)) + (intJtoFa _M3)
faJP k@(AbstractPitch2 C Na) (AbstractPitch2 (Up C) Na)      = intJtoFa _P8

faJP k (AbstractPitch2 n (Sh a)) = (faJP k (AbstractPitch2 n a)) + (intJtoFa _A1)
faJP k (AbstractPitch2 n (Fl a)) = (faJP k (AbstractPitch2 n a)) - (intJtoFa _A1)

faJP k (AbstractPitch2 (Up n) a)   = (faJP k (AbstractPitch2 n a)) + (intJtoFa _P8)
faJP k (AbstractPitch2 (Down n) a) = (faJP k (AbstractPitch2 n a)) - (intJtoFa _P8)

faJP k p = let i = p .-. k
               p' = c .+^ i
               i' = faJP c p'
           in (faJP c k) + i'
-- e.g. (faJP d g) - (faJP d d) == (faJP c f) - (faJP c c), i.e the P4
-- between c and f, in c major, is the same as the P4 between d and g
-- in d major (but not the same as the P4 between d and g in c major).

jpToFa (JustPitch k p) = faJP k p

oct m = m `div` 7

fa3Norm (m,n,p) = (m - 3*(oct p), n - 12*(oct p), (p `mod` 7))

toJP k (FA3 (m,n,p)) = JustPitch k $ AbstractPitch2 (toEnum (p + 2)) ((fa3ToAcc . fa3Norm) (m,n,p))

-- This deliberately throws away comma (the 'm' in (m,n,p))
-- information, and refuses to let you change key just by transposing
-- by some weird (i.e. acute- or grave-) interval. Bad luck.

fa3ToAcc (m,n,p)
  | (n < 0) && (p == 0) = Fl (fa3ToAcc (m, (n + 1), p))
  | (n, p) == (0, 0) = Na
  | (n > 0) && (p == 0) = Sh (fa3ToAcc (m, (n - 1), p))
  | (n < 2) && (p == 1) = Fl (fa3ToAcc (m, (n + 1), p))
  | (n, p) == (2, 1) = Na
  | (n > 2) && (p == 1) = Sh (fa3ToAcc (m, (n - 1), p))
  | (n < 4) && (p == 2) = Fl (fa3ToAcc (m, (n + 1), p))
  | (n, p) == (4, 2) = Na
  | (n > 4) && (p == 2) = Sh (fa3ToAcc (m, (n - 1), p))
  | (n < 5) && (p == 3) = Fl (fa3ToAcc (m, (n + 1), p))
  | (n, p) == (5, 3) = Na
  | (n > 5) && (p == 3) = Sh (fa3ToAcc (m, (n - 1), p))
  | (n < 7) && (p == 4) = Fl (fa3ToAcc (m, (n + 1), p))
  | (n, p) == (7, 4) = Na
  | (n > 7) && (p == 4) = Sh (fa3ToAcc (m, (n - 1), p))
  | (n < 9) && (p == 5) = Fl (fa3ToAcc (m, (n + 1), p))
  | (n, p) == (9, 5) = Na
  | (n > 9) && (p == 5) = Sh (fa3ToAcc (m, (n - 1), p))
  | (n < 11) && (p == 6) = Fl (fa3ToAcc (m, (n + 1), p))
  | (n, p) == (11, 6) = Na
  | (n > 11) && (p == 6) = Sh (fa3ToAcc (m, (n - 1), p))
  | (n < 12) && (p == 7) = Fl (fa3ToAcc (m, (n + 1), p))
  | (n, p) == (12, 7) = Na
  | (n > 12) && (p == 7) = Sh (fa3ToAcc (m, (n - 1), p))


int = JustInt
d1 = int (Dim Perf) Unison
_P1 = int Perf Unison
comma = int (Acute Perf) Unison
_A1 = int (Aug Perf) Unison
d2 = int (Dim Min) Second
m2 = int Min Second
_L2 = int Lesser Second
_G2 = int Greater Second
_A2 = int (Aug Greater) Second
d3 = int (Dim Min) Third
m3 = int Min Third
_M3 = int Maj Third
_A3 = int (Aug Maj) Third
d4 = int (Dim Perf) Fourth
_P4 = int Perf Fourth
_A4 = int (Aug Perf) Fourth
d5 = int (Dim Perf) Fifth
_P5 = int Perf Fifth
_A5 = int (Aug Perf) Fifth
d6 = int (Dim Min) Sixth
m6 = int Min Sixth
_M6 = int Maj Sixth
_A6 = int (Aug Maj) Sixth
d7 = int (Dim Min) Seventh
m7 = int Min Seventh
_M7 = int Maj Seventh
_A7 = int (Aug Maj) Seventh
d8 = int (Dim Perf) (Compound Unison)
_P8 = int Perf (Compound Unison)
_A8 = int (Aug Perf) (Compound Unison)


-- m2 = 16/15
rat_m2 = AbstractInt3 $ 16/15
-- L2 = 10/9
rat_L2 = AbstractInt3 $ 10/9
-- G2 = 9/8
rat_G2 = AbstractInt3 $ 9/8

-- Ac1 = G2 - L2
rat_Ac1 = rat_G2 ^-^ rat_L2

-- A1 = L2 - m2
rat_A1 = rat_L2 ^-^ rat_m2

-- d2 = m2 - A1
rat_d2 = rat_m2 ^-^ rat_A1

data JustTuning = JustTuning (JustPitch, AbstractPitch3) deriving Show
instance Tuning JustTuning JustPitch JustInt where
  base (JustTuning b) = b
  tuneInt _ = justTune rat_Ac1 rat_A1 rat_d2

data ForceJustTuning = ForceJustTuning (Key,AbstractPitch2,AbstractPitch3) deriving Show
instance Tuning ForceJustTuning AbstractPitch2 AbstractInt2 where
  base (ForceJustTuning (k,b,f)) = (b,f)
  tune (ForceJustTuning (k,b,f)) p = tune (JustTuning (JustPitch k b,f)) (JustPitch k p)
  tuneInt t i = let (b, f) = base t
                    b' = b .+^ i
                in (tune t b') .-. (tune t b)


t = JustTuning (JustPitch a a, AbstractPitch3 440)

me = Metronome 240

example = Start $ phrase $ zipWith note [jp c c, jp c d, jp c e, jp c f, jp c g] (repeat cr)

perform = (mapMusic $ mapPhrase $ noteToSound t me) example

