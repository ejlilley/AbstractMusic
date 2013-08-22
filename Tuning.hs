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

module Tuning where

import Prelude hiding (negate)

import Util (interleave)

import Music (Name(..), Accidental(..),
              Quality(..), Number(..),
              AbstractPitch3(..), AbstractInt3(..), AbstractDur3(..),
              AbstractPitch2(..), AbstractInt2(..), AbstractDur2(..),
              AbstractNote(..), Note, Tuning(..), Transpose(..),
              Freq, add, sub, invert, negate, transpose, interval, octave,
              faInt, faPitch, FreeAbelian(..), intToFa, pitchToFa, toInterval, toPitch)
import Shortcuts

import Control.Monad
import Data.Ratio

data Pythagorean = Pythagorean (AbstractPitch2, AbstractPitch3) deriving Show

pythagd2 = 524288%531441 -- is equal to comma^(-1), i.e. in negative direction
pythagA1 = 2187%2048 -- chromatic semitone (actually larger than m2, the diatonic semitone)

instance Tuning Pythagorean AbstractPitch2 AbstractInt2 where
  base (Pythagorean b) = b
  tune (Pythagorean (p, base)) p' = let (AbstractInt2 q n) = interval p p'
                                        a1 ::+ d2 = faInt q n
                                        d2s = (fromRational pythagd2) ** (fromIntegral d2)
                                        a1s = (fromRational pythagA1) ** (fromIntegral a1)
                                        f = d2s * a1s
                                        AbstractPitch3 b = base
                                    in AbstractPitch3 $ b * f

equalST = 2 ** (1/12)

data Equal = Equal (AbstractPitch2, AbstractPitch3) deriving Show

instance Tuning Equal AbstractPitch2 AbstractInt2 where
  base (Equal b) = b
  tune (Equal (p, b)) p' = let (AbstractInt2 q n) = interval p p'
                               a1 ::+ d2 = faInt q n
                               AbstractPitch3 b' = b
                           in AbstractPitch3 $ b' * (equalST ** (fromIntegral a1))

test (Equal (p, b)) p' = let (AbstractInt2 q n) = interval p p'
                             a1 ::+ d2 = faInt q n
                             AbstractPitch3 b' = b
--                         in AbstractPitch3 $ b' * (equalST ** (fromIntegral a1))
                         in a1 ::+ d2





qcdiesis = 128%125

qcchrom = ((125/128)**2 * 5/4)**(1/4)

data QCMeanTone = QCMeanTone (AbstractPitch2, AbstractPitch3) deriving Show

instance Tuning QCMeanTone AbstractPitch2 AbstractInt2 where
  base (QCMeanTone b) = b
  tune (QCMeanTone (p, base)) p' = let (AbstractInt2 q n) = interval p p'
                                       a1 ::+ d2 = faInt q n
                                       dieses = (fromRational qcdiesis) ** (fromIntegral d2)
                                       chroms = qcchrom ** (fromIntegral a1)
                                       f = dieses * chroms
                                       AbstractPitch3 b = base
                                   in AbstractPitch3 $ b * f



-- data Ptolemy
--
type TET12 = Equal
-- data TET19
--
-- data TET31
--
-- data Just3Limit
-- 
-- data Just5Limit
-- 
