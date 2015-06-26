{-# LANGUAGE GADTs,
             MultiParamTypeClasses #-}


module Scales where


import Music (Scale(..),
              AbstractPitch1(..), AbstractInt1(..),
              AbstractPitch2(..), AbstractInt2(..),
              Interval(..), Pitch(..), Transpose(..),
              faInt, faPitch,
              Name(..), Number(..), Quality(..), Accidental(..), Ficta(..))
import Shortcuts

import Util (rotate, rotateN)

data GenericScale where
  GenericScale :: Scale s p i => s -> GenericScale

-- todo: represent/enforce scale length(s) with type-level Nats.

-- todo: make the basic scale type a list of *intervals* (not pitches). e.g. baseMajor = [M2, M2, m2, M2, M2, M2, m2] etc.

ficToAcc Raise = sharpen
ficToAcc Neutral = id
ficToAcc Lower = flatten

completeScale s i = let c = if i >= 0
                            then scale s ++
                                 map (transpose (AbstractInt2 Perf (Compound Unison))) c
                            else map (transpose (AbstractInt2 Perf (Negative (Compound Unison)))) (reverse (scale s)) ++
                                 map (transpose (AbstractInt2 Perf (Negative (Compound Unison)))) c
                    in if i >= 0
                       then c
                       else (head (scale s)) : c

infiniteScale s = completeScale s 1

scaleDegree s (AbstractPitch1 deg fic) =
  let i = fromEnum deg
      index = abs i
      note = (completeScale s i) !! index
  in (ficToAcc fic) note


-- Ionian | Hypoionian | Aeolian | Hypoaeolian | Dorian | Phrygian | Lydian | Mixolydian | Hypodorian | Hypophrygian | Hypolydian | Hypomixolydian | Locrian | Hypolocrian

transposeScale orig base new = let offset = interval base new
                               in map (transpose offset) orig

-- Diatonic:

basicIonian = map (\n -> AbstractPitch2 n Na) [C .. ]

data Ionian = Ionian AbstractPitch2 deriving Show
type Major = Ionian
instance Scale Ionian AbstractPitch1 AbstractInt1 where
  tonic (Ionian t) = t
  scale s = take 7 $ transposeScale basicIonian (AbstractPitch2 C Na) (tonic s)
  applyScale = scaleDegree

data Dorian = Dorian AbstractPitch2 deriving Show
instance Scale Dorian AbstractPitch1 AbstractInt1 where
  tonic (Dorian t) = t
  scale s = take 7 $ transposeScale (rotate basicIonian) (AbstractPitch2 D Na) (tonic s)
  applyScale = scaleDegree

data Phrygian = Phrygian AbstractPitch2 deriving Show
instance Scale Phrygian AbstractPitch1 AbstractInt1 where
  tonic (Phrygian t) = t
  scale s = take 7 $ transposeScale ((rotateN 2) basicIonian) (AbstractPitch2 E Na) (tonic s)
  applyScale = scaleDegree

data Lydian = Lydian AbstractPitch2 deriving Show
instance Scale Lydian AbstractPitch1 AbstractInt1 where
  tonic (Lydian t) = t
  scale s = take 7 $ transposeScale ((rotateN 3) basicIonian) (AbstractPitch2 F Na) (tonic s)
  applyScale = scaleDegree

data Mixolydian = Mixolydian AbstractPitch2 deriving Show
instance Scale Mixolydian AbstractPitch1 AbstractInt1 where
  tonic (Mixolydian t) = t
  scale s = take 7 $ transposeScale ((rotateN 4) basicIonian) (AbstractPitch2 G Na) (tonic s)
  applyScale = scaleDegree

data Aeolian = Aeolian AbstractPitch2 deriving Show
type Minor = Aeolian
instance Scale Aeolian AbstractPitch1 AbstractInt1 where
  tonic (Aeolian t) = t
  scale s = take 7 $ transposeScale ((rotateN 5) basicIonian) (AbstractPitch2 A Na) (tonic s)
  applyScale = scaleDegree

data Locrian = Locrian AbstractPitch2 deriving Show
instance Scale Locrian AbstractPitch1 AbstractInt1 where
  tonic (Locrian t) = t
  scale s = take 7 $ transposeScale ((rotateN 6) basicIonian) (AbstractPitch2 B Na) (tonic s)
  applyScale = scaleDegree

-- Melodic minor scales:

basicMelodicMinor = [AbstractPitch2 C Na,
                     AbstractPitch2 D Na,
                     AbstractPitch2 E flat,
                     AbstractPitch2 F Na,
                     AbstractPitch2 G Na,
                     AbstractPitch2 (Up A) Na,
                     AbstractPitch2 (Up B) Na] ++ map (transpose (AbstractInt2 Perf (Compound Unison))) basicMelodicMinor

data MelodicMinor = MelodicMinor AbstractPitch2 deriving Show
instance Scale MelodicMinor AbstractPitch1 AbstractInt1 where
  tonic (MelodicMinor t) = t
  scale s = take 7 $ transposeScale basicMelodicMinor (AbstractPitch2 C Na) (tonic s)
  applyScale = scaleDegree

-- Harmonic major scales:

basicHarmonicMajor = [AbstractPitch2 C Na,
                      AbstractPitch2 D Na,
                      AbstractPitch2 E Na,
                      AbstractPitch2 F Na,
                      AbstractPitch2 G Na,
                      AbstractPitch2 (Up A) flat,
                      AbstractPitch2 (Up B) Na] ++ map (transpose (AbstractInt2 Perf (Compound Unison))) basicHarmonicMajor


-- Harmonic minor scales:

basicHarmonicMinor = [AbstractPitch2 C Na,
                      AbstractPitch2 D Na,
                      AbstractPitch2 E flat,
                      AbstractPitch2 F Na,
                      AbstractPitch2 G Na,
                      AbstractPitch2 (Up A) flat,
                      AbstractPitch2 (Up B) Na] ++ map (transpose (AbstractInt2 Perf (Compound Unison))) basicHarmonicMinor

data AlteredPhrygian = AlteredPhrygian AbstractPitch2 deriving Show
instance Scale AlteredPhrygian AbstractPitch1 AbstractInt1 where
  tonic (AlteredPhrygian t) = t
  scale s = take 7 $ transposeScale (rotateN 4 basicHarmonicMinor) (AbstractPitch2 G Na) (tonic s)
  applyScale = scaleDegree

data HarmonicMinor = HarmonicMinor AbstractPitch2 deriving Show
instance Scale HarmonicMinor AbstractPitch1 AbstractInt1 where
  tonic (HarmonicMinor t) = t
  scale s = take 7 $ transposeScale basicHarmonicMinor (AbstractPitch2 C Na) (tonic s)
  applyScale = scaleDegree



-- Double harmonic scales:

basicDoubleHarmonic = [AbstractPitch2 C Na,
                       AbstractPitch2 D flat,
                       AbstractPitch2 E Na,
                       AbstractPitch2 F Na,
                       AbstractPitch2 G Na,
                       AbstractPitch2 (Up A) flat,
                       AbstractPitch2 (Up B) Na] ++ map (transpose (AbstractInt2 Perf (Compound Unison))) basicDoubleHarmonic



major :: AbstractPitch2 -> Major
major n = Ionian n
minor :: AbstractPitch2 -> Minor
minor n = Aeolian (n .-^ octave)
harmonicminor :: AbstractPitch2 -> HarmonicMinor
harmonicminor n = HarmonicMinor n
melodicminor :: AbstractPitch2 -> MelodicMinor
melodicminor n = MelodicMinor n

chromaticScale p@(AbstractPitch2 n a)
  | (n == B) || (n == E) || (a == sharp) = p:(chromaticScale (AbstractPitch2 (succ n) Na))
  | otherwise = p:(chromaticScale (AbstractPitch2 n sharp))

-- Modal:

-- modeII = [(D, Na), (E, Na), (F, Na), (G, Na), (A, Na), (B, Na), (C, Na)]
-- modeIII
-- modeIV
-- modeV
-- modeVI
-- modeVII
-- modeVIII


-- Messiaen's scales:

--        mode1 = [2,2,2,2,2]
--        mode2 = [1,2, 1,2, 1,2, 1,2]
--        mode3 = [2,1,1, 2,1,1, 2,1,1]
--        mode4 = [1,1,3,1,   1,1,3,1]
--        mode5 = [1,4,1,     1,4,1]
--        mode6 = [2,2,1,1,   2,2,1,1]
--        mode7 = [1,1,1,2,1, 1,1,1,2,1]

-- (measured in semitones)



