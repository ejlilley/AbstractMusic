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

module Tuning (Tuning(..),
               Pythagorean(..),
               Equal(..),
               QCMeanTone(..),
               SeptimalMeanTone(..),
               TET7(..),
               TET12(..),
               TET17(..),
               TET19(..),
               TET22(..),
               TET24(..),
               TET31(..),
               TET54(..),
               TET72(..),
               DummyTuning(..)) where

import Prelude hiding (negate)

import Util (interleave)

import Music (Name(..), Accidental(..),
              Quality(..), Number(..),
              AbstractPitch3(..), AbstractInt3(..), AbstractDur3(..),
              AbstractPitch2(..), AbstractInt2(..), AbstractDur2(..),
              AbstractNote(..), Note, Tuning(..), Transpose(..),
              Freq, add, sub, invert, negate, transpose, interval, octave,
              faInt, faPitch, FreeAbelian(..), intToFa, pitchToFa, toInterval, toPitch)

import Algebra
import Shortcuts

-- this doesn't work... learn about Type Functions first.
-- type TuningSystem a = a (AbstractPitch2, AbstractPitch3)

-- return an equal division of the octave
edo :: Int -> AbstractInt3
edo n = octave ^* (1/(fromIntegral n))

-- Tunings based on EDO -- i.e. one generator for the whole scale, but
-- utilising the representation of intervals as n*A1 + m*d2 (by some
-- linear transformation) to number the resulting degrees of the
-- scale.
edoTune :: AbstractInt3 -> (Int, Int) -> AbstractInt2 -> AbstractInt3
edoTune d (x, y) (AbstractInt2 q n) =
  let a ::+ b = faInt q n
  in (fromIntegral (x*a + y*b)) *^ d

-- Tunings based on fixing the value of two intervals -- i.e. two
-- generators for the whole scale.
pureTune :: AbstractInt3 -> AbstractInt3 -> AbstractInt2 -> AbstractInt3
pureTune a1rat d2rat (AbstractInt2 q n) =
  let a1 ::+ d2 = faInt q n
  in (a1rat ^* (fromIntegral a1)) ^+^ (d2rat ^* (fromIntegral d2))


makeBasis i (i1, r1) (i2, r2) = case (intervalDivisors i i1 i2) of
  Just (x, y) -> AbstractInt3 $ r1**(fromIntegral x) * r2**(fromIntegral y)
  Nothing -> error ("Cannot use intervals " ++ (show i1) ++ " and " ++ (show i2) ++ " as basis pair to represent " ++ (show i))
-- possibly todo: make a version of intervalDivisors that's allowed to
-- return non-integers, so we don't sometimes have to do some manual
-- calculating (see below).

makeA1 = makeBasis _A1
maked2 = makeBasis d2

data Pythagorean = Pythagorean (AbstractPitch2, AbstractPitch3) deriving Show

pythag_A1 = makeA1 (_P8, 2) (_P5, 3/2) -- is equal to comma^(-1), i.e. in negative direction
pythag_d2 = maked2 (_P8, 2) (_P5, 3/2) -- chromatic semitone (is larger than m2, the diatonic semitone)

instance Tuning Pythagorean AbstractPitch2 AbstractInt2 where
  base (Pythagorean b) = b
  tuneInt _ = pureTune pythag_A1 pythag_d2


data Equal = Equal (AbstractPitch2, AbstractPitch3) deriving Show
type TET12 = Equal
instance Tuning Equal AbstractPitch2 AbstractInt2 where
  base (Equal b) = b
  tuneInt _ = edoTune (edo 12) (1, 0)



data QCMeanTone = QCMeanTone (AbstractPitch2, AbstractPitch3) deriving Show

qcd2 = maked2 (_P8, 2) (_M3, 5/4) -- (diesis)
-- M3 & P8 are *not* a basis pair, as they cannot represent A1 with
-- integer coefficients. So, we exploit the fact that:
--     M3 = 4*A1 + 2*d2
-- ==> A1 = (1/4) * (M3 - 2*d2)
-- And the resulting A1 & d2 are irrational.
qcM3 = AbstractInt3 $ 5/4
qcA1 = (1/4) *^ (qcM3 ^-^ (2 *^ qcd2))

instance Tuning QCMeanTone AbstractPitch2 AbstractInt2 where
  base (QCMeanTone b) = b
  tuneInt _ = pureTune qcA1 qcd2




data SeptimalMeanTone = SeptimalMeanTone (AbstractPitch2, AbstractPitch3)
                      deriving Show

-- Once again, exploit some properties to derive the (irrational)
-- values for A1 and d2.
smtA6 = AbstractInt3 $ 7/4
smtA1 = (1/10) *^ ((7 *^ smtA6) ^-^ (5 *^ octave))
smtd2 = (1/7) *^ (octave ^-^ (12 *^ smtA1))

instance Tuning SeptimalMeanTone AbstractPitch2 AbstractInt2 where
  base (SeptimalMeanTone b) = b
  tuneInt _ = pureTune smtA1 smtd2


data TET19 = TET19 (AbstractPitch2, AbstractPitch3) deriving Show

instance Tuning TET19 AbstractPitch2 AbstractInt2 where
  base (TET19 b) = b
  tuneInt _ = edoTune (edo 19) (1, 1)


data TET7 = TET7 (AbstractPitch2, AbstractPitch3) deriving Show
instance Tuning TET7 AbstractPitch2 AbstractInt2 where
  base (TET7 b) = b
  tuneInt _ = edoTune (edo 7) (0, 1)

data DummyTuning = DummyTuning (AbstractPitch2, AbstractPitch3) deriving Show
instance Tuning DummyTuning AbstractPitch2 AbstractInt2 where
  base (DummyTuning b) = b
  tuneInt _ _ = AbstractInt3 1


-- data TET17 -- todo: implement the arabic system of notation/scales,
-- then use TET17 as its tuning system.
data TET17 = TET17 (AbstractPitch2, AbstractPitch3) deriving Show
instance Tuning TET17 AbstractPitch2 AbstractInt2 where
  base (TET17 b) = b
  tuneInt _ = edoTune (edo 17) (2, -1)

-- data Indian22 -- todo: implement the Indian system of notation/scales,
-- then use this as its tuning system.

-- the EDO-version of Indian22 above (maybe)
data TET22 = TET22 (AbstractPitch2, AbstractPitch3) deriving Show
instance Tuning TET22 AbstractPitch2 AbstractInt2 where
  base (TET22 b) = b
  tuneInt _ = edoTune (edo 22) (3, -2)

-- TET12 + quarter-tones:
data TET24 = TET24 (AbstractPitch2, AbstractPitch3) deriving Show
instance Tuning TET24 AbstractPitch2 AbstractInt2 where
  base (TET24 b) = b
  tuneInt _ = edoTune (edo 24) (2, 0)


data TET31 = TET31 (AbstractPitch2, AbstractPitch3) deriving Show
instance Tuning TET31 AbstractPitch2 AbstractInt2 where
  base (TET31 b) = b
  tuneInt _ = edoTune (edo 31) (2, 1)



-- fixme: the current choices of (x,y) for TET54 and TET72 are
-- obviously nonsense!

data TET54 = TET54 (AbstractPitch2, AbstractPitch3) deriving Show
instance Tuning TET54 AbstractPitch2 AbstractInt2 where
  base (TET54 b) = b
  tuneInt _ = edoTune (edo 54) (1, 6)

-- TET12 divided by 6:
data TET72 = TET72 (AbstractPitch2, AbstractPitch3) deriving Show
instance Tuning TET72 AbstractPitch2 AbstractInt2 where
  base (TET72 b) = b
  tuneInt _ = edoTune (edo 72) (6, 0)


-- Zarlino's Just scale uses: makeBasis i (m3, 6/5) (_M3, 5/4)



-- data Ptolemy
--
-- data Just3Limit
-- 
-- data Just5Limit
-- 
