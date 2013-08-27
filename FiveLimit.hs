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

import Music (Quality(..), Number(..),
              Tuning(..), AbstractPitch2(..), AbstractPitch3(..),
              AbstractInt2(..), AbstractInt3(..), Scale(..),
              Interval(..), Transpose(..), Pitch(..),
              Name(..), Accidental(..))

import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace

import Scales (major, minor, infiniteScale)
import Shortcuts



data PtolemyIntense = PtolemyIntense (AbstractPitch2, AbstractPitch3)

instance Tuning PtolemyIntense AbstractPitch2 AbstractInt2 where
  base (PtolemyIntense b) = b
  tuneInt _ = undefined
  tune t@(PtolemyIntense (b, f)) p
    | (i < _P1) = (tune t (p .+^ _P8)) .-^ octave
    | (i == _P1) = f
    | (i == _M2) = f .+^ (AbstractInt3 $ 9/8)
    | (i == _M3) = f .+^ (AbstractInt3 $ 5/4)
    | (i == _P4) = f .+^ (AbstractInt3 $ 4/3)
    | (i == _P5) = f .+^ (AbstractInt3 $ 3/2)
    | (i == _M6) = f .+^ (AbstractInt3 $ 5/3)
    | (i == _M7) = f .+^ (AbstractInt3 $ 15/8)
    | (i == _P8) = f .+^ octave
    | (i > _P8) = (tune t (p .-^ _P8)) .+^ octave
    | otherwise = error "Can only use PtolemyIntense for diatonic major scales"
      where i = p .-. b


data FreeAbelian3 = FA3 (Int, Int, Int)
-- Rank 3 free Abelian group, (s,t,T) ∊ ℤ×ℤ×ℤ,
-- measuring (semitones, minor tones, major tones).

instance Num FreeAbelian3 where
  (FA3 (a,b,c)) + (FA3 (d,e,f)) = FA3 (a+d,b+e,c+f)
  (FA3 (a,b,c)) - (FA3 (d,e,f)) = FA3 (a-d,b-e,c-f)
  (FA3 (a,b,c)) * (FA3 (d,e,f)) = FA3 (a*d,b*e,c*f)
  fromInteger _ = undefined
  abs _ = undefined
  signum _ = undefined

instance Show FreeAbelian3 where
  show (FA3 (m,n,p)) = "<" ++ (show m) ++ "," ++ (show n) ++ "," ++ (show p) ++ ">"

-- 
-- data JustInt = JustInt Width Quality Number deriving Show
-- 
-- data Width = Lesser | Grave | Acute | Greater deriving Show -- need a fourth one
-- 
-- 
-- faIntJ Neutral Perf Unison = FA3 (0,0,0)
-- faIntJ Lesser (Aug Perf) Unison = FA3 (-1,1,0)
-- faIntJ Greater (Aug Perf) Unison = FA3 (-1,0,1)
-- 
-- faIntJ Lesser (Dim Min) Second = FA3 (-1,1,0)
-- faIntJ Greater (Dim Min) Second = FA3 (1,-1,0)
-- 
-- faIntJ Neutral Min Second = FA3 (1,0,0)
-- faIntJ Lesser Maj Second = FA3 (0,1,0)
-- faIntJ Greater Maj Second = FA3 (0,0,1)
-- 
-- faIntJ Neutral Min Third = FA3 (1,0,1)
-- faIntJ Neutral Maj Third = FA3 (0,1,1)
-- 
-- faIntJ Neutral Perf (Compound Unison) = FA3 (2,2,3)
-- 
-- faIntJ w (Dim q) n = (faIntJ w q n) - (FA3 (-1,0,1))
-- faIntJ w (Aug q) n = (faIntJ w q n) + (FA3 (-1,0,1))

type Key = AbstractPitch2

data JustPitch = JustPitch Key AbstractPitch2 deriving (Show, Eq)
data JustInt = JustInt Key AbstractInt2 deriving (Show, Eq)

-- Note that (Key, Pitch, Accidental) aren't *completely* orthogonal,
-- but they still (hopefully) determine the FA3 for that pitch
-- uniquely.


instance AdditiveGroup JustInt where

instance AffineSpace JustPitch where
  type Diff JustPitch = JustInt

instance Transpose JustPitch JustInt where

instance VectorSpace JustInt where

instance Interval JustInt where

instance Pitch JustPitch where
--  sharpen (JustPitch k n a) = toPitch $ (faPitch3 k n a) + (FA3 (-1,0,1))
--  flatten (JustPitch k n a) = toPitch $ (faPitch3 k n a) - (FA3 (-1,0,1))
  -- incr (JustPitch k n a) = toPitch $ (faPitch3 k n a) + (0 ::+ 1)
  -- decr (JustPitch k n a) = toPitch $ (faPitch3 k n a) - (0 ::+ 1)
  middle = JustPitch c c


_s = FA3 (1,0,0)
_t = FA3 (0,1,0)
_T = FA3 (0,0,1)
_oct = _T + _T + _T + _t + _t + _s + _s
_aug = FA3 (-1,0,1)

faJI :: JustInt -> FreeAbelian3 -- currently just for symmetric scale 1.

faJI (JustInt (AbstractPitch2 C Na) (AbstractInt2 Perf Unison)) = FA3 (0,0,0)
faJI (JustInt k i) = let base = faJP (JustPitch k k)
                         p = faJP $ (JustPitch k (k .+^ i))
                     in p - base


faJP :: JustPitch -> FreeAbelian3 -- currently just for symmetric scale 1.

faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 A a)) = (faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 (Up A) a))) - _oct
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 B a)) = (faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 (Up A) a))) - _oct

faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 C Na))      = FA3 (0,0,0)
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 D (Fl Na))) = _s
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 D Na))      = _T
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 E (Fl Na))) = _T + _s
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 E Na))      = _T + _t
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 F Na))      = _T + _t + _s
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 F (Sh Na))) = _T + _T + _t
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 G (Fl Na))) = _T + _t + _s + _s
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 G Na))      = _T + _T + _t + _s
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 (Up A) (Fl Na))) = _T + _T + _t + _s + _s
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 (Up A) Na))      = _T + _T + _t + _t + _s
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 (Up B) (Fl Na))) = _T + _T + _t + _t + _s + _s
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 (Up B) Na))      = _T + _T + _T + _t + _t + _s
faJP (JustPitch (AbstractPitch2 C Na) (AbstractPitch2 (Up C) Na))      = _oct

faJP (JustPitch k (AbstractPitch2 n (Sh a))) = (faJP (JustPitch k (AbstractPitch2 n a))) + _aug
faJP (JustPitch k (AbstractPitch2 n (Fl a))) = (faJP (JustPitch k (AbstractPitch2 n a))) - _aug

faJP (JustPitch k (AbstractPitch2 (Up n) a))   = (faJP (JustPitch k (AbstractPitch2 n a))) + _oct
faJP (JustPitch k (AbstractPitch2 (Down n) a)) = (faJP (JustPitch k (AbstractPitch2 n a))) - _oct

faJP (JustPitch k p) = let base = faJP (JustPitch c k)
                           pitch = faJP (JustPitch c p)
                           in pitch - base -- relative to k






rat_s = AbstractInt3 $ 16/15
rat_t = AbstractInt3 $ 10/9
rat_T = AbstractInt3 $ 9/8

data FiveLimitSymmetric1 = FiveLimitSymmetric1 (JustPitch, AbstractPitch3) deriving Show

instance Tuning FiveLimitSymmetric1 JustPitch JustInt where
  base (FiveLimitSymmetric1 b) = b -- fixme: currently ignores base, effectively using (JustPitch a a) as the base every time
  tuneInt _ = undefined
  tune t@(FiveLimitSymmetric1 (b, f)) pitch = let (FA3 (m,n,p)) = faJP pitch
                                          in f .+^ (((fromIntegral m) *^ rat_s) ^+^ ((fromIntegral n) *^ rat_t) ^+^ ((fromIntegral p) *^ rat_T))


t = FiveLimitSymmetric1 (JustPitch c c, freq 262)






