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
              countComp, countNeg, justNum)

import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace

import Scales (major, minor, infiniteScale)

justTune :: AbstractInt3 -> AbstractInt3 -> AbstractInt3 -> JustInt -> AbstractInt3
justTune ac1rat a1rat d2rat (JustInt qu nu) =
  let FA3 (m,n,p) = faIntJ qu nu
  in (ac1rat ^* (fromIntegral m)) ^+^ (a1rat ^* (fromIntegral n)) ^+^ (d2rat ^* (fromIntegral p))


-- data PtolemyIntense = PtolemyIntense (AbstractPitch2, AbstractPitch3)
-- 
-- instance Tuning PtolemyIntense AbstractPitch2 AbstractInt2 where
--   base (PtolemyIntense b) = b
--   tuneInt _ = undefined
--   tune t@(PtolemyIntense (b, f)) p
--     | (i < _P1) = (tune t (p .+^ _P8)) .-^ octave
--     | (i == _P1) = f
--     | (i == _M2) = f .+^ (AbstractInt3 $ 9/8)
--     | (i == _M3) = f .+^ (AbstractInt3 $ 5/4)
--     | (i == _P4) = f .+^ (AbstractInt3 $ 4/3)
--     | (i == _P5) = f .+^ (AbstractInt3 $ 3/2)
--     | (i == _M6) = f .+^ (AbstractInt3 $ 5/3)
--     | (i == _M7) = f .+^ (AbstractInt3 $ 15/8)
--     | (i == _P8) = f .+^ octave
--     | (i > _P8) = (tune t (p .-^ _P8)) .+^ octave
--     | otherwise = error "Can only use PtolemyIntense for diatonic major scales"
--       where i = p .-. b


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

data JustPitch = JustPitch Key AbstractPitch2 deriving (Eq)

instance Show JustPitch where
  show (JustPitch k p) = "[" ++ (show p) ++ " in " ++ (show k) ++ "]"

-- data JustInt = JustInt Key AbstractInt2 deriving (Show, Eq)
data JustInt = JustInt JustQuality Number deriving (Eq)

instance Show JustInt where
  show (JustInt q n) = (show q) ++ (show n)

instance Ord JustInt where
  (JustInt _ n) `compare` (JustInt _ m) = (fromEnum n) `compare` (fromEnum m)

instance Ord JustPitch where
  (JustPitch _ (AbstractPitch2 n _)) `compare` (JustPitch _ (AbstractPitch2 m _)) =
    (fromEnum n) `compare` (fromEnum m)

-- data Width = Lesser | Classic | Greater | Acute | Grave | Wide | Semi deriving (Show, Eq)

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
  show (Aug q) = "A" ++ (show q)
  show (Dim q) = "d" ++ (show q)
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
  transpose (JustInt q i) (JustPitch n a) = undefined
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
  sharpen (JustPitch k p) = undefined
  flatten (JustPitch k p) = undefined
  incr (JustPitch k (AbstractPitch2 n a)) = JustPitch k $ AbstractPitch2 (succ n) a
  decr (JustPitch k (AbstractPitch2 n a)) = JustPitch k $ AbstractPitch2 (pred n) a
  middle = JustPitch (AbstractPitch2 A Na) (AbstractPitch2 A Na)



jAc1 = FA3 (1,0,0) -- just comma
jA1 = FA3 (0,1,0) -- just augmented unison
jd2 = FA3 (0,0,1) -- just diminished second

jWA1 = jGM2 - jm2

-- Note, as far as we're concerned, WA(i) (wide augmented) == AcA(i) (acute augmented)

jm2 = jd2 + jA1

-- dim(i) = i - A1
-- aug(i) = i + A1
-- acute(i) = i + c
-- grave(i) = i - c


jAcm2 = jm2 + jAc1
jdGM2 = jGM2 - jA1
-- jdGM2 = jAcm2
-- so, acute(m2) = dim(GM2)

jGrM2 = jLM2 - jAc1

jLM2 = jm2 + jA1
jGM2 = jLM2 + jAc1

jALM2 = jLM2 + jA1
jAGM2 = jGM2 + jA1

-- natural intervals can have either:
-- Perf (1,4,5)
-- Min Lesser Greater (2) (??)
-- Min Maj (3,6,7) (??)

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
-- faJP k@(AbstractPitch2 C Na) (AbstractPitch2 F (Sh Na))      = 
-- faJP k@(AbstractPitch2 C Na) (AbstractPitch2 G (Fl Na))      = 
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


pitch :: Name -> Accidental -> AbstractPitch2
pitch n a = AbstractPitch2 n a

sharp = Sh Na
dsharp = Sh sharp
flat = Fl Na
dflat = Fl flat
natural = Na

aeses = pitch A dflat
aes   = pitch A flat
a     = pitch A natural
ais   = pitch A sharp
aisis = pitch A dsharp

beses = pitch B dflat
bes   = pitch B flat
b     = pitch B natural
bis   = pitch B sharp
bisis = pitch B dsharp

ceses = pitch C dflat  
ces   = pitch C flat   
c     = pitch C natural
cis   = pitch C sharp  
cisis = pitch C dsharp 

deses = pitch D dflat  
des   = pitch D flat   
d     = pitch D natural
dis   = pitch D sharp  
disis = pitch D dsharp 

eeses = pitch E dflat  
ees   = pitch E flat   
e     = pitch E natural
eis   = pitch E sharp  
eisis = pitch E dsharp 

feses = pitch F dflat  
fes   = pitch F flat   
f     = pitch F natural
fis   = pitch F sharp  
fisis = pitch F dsharp 

geses = pitch G dflat  
ges   = pitch G flat   
g     = pitch G natural
gis   = pitch G sharp  
gisis = pitch G dsharp 

noteList = [aeses, aes, a, ais, aisis,
            beses, bes, b, bis, bisis,
            ceses, ces, c, cis, cisis,
            deses, des, d, dis, disis,
            eeses, ees, e, eis, eisis,
            feses, fes, f, fis, fisis,
            geses, ges, g, gis, gisis]


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
--  tuneInt (ForceJustTuning (k,b,f)) i = tuneInt (JustTuning (JustPitch k b,f)) i
  tuneInt (ForceJustTuning (k,b,f)) i = undefined

t = JustTuning (JustPitch a a, AbstractPitch3 440)






instance Note JustPitch JustInt AbstractDur2 where



