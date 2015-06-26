module Main where

import Music (Name(..), Accidental(..), Scale(..), Tuning(..), Timing(..), Metronome(..),
              AbstractInt1(..), AbstractPitch1(..), AbstractDur1(..),
              AbstractInt2(..), AbstractPitch2(..), AbstractDur2(..),
              AbstractInt3(..), AbstractPitch3(..), AbstractDur3(..),
              Name(..), Number(..), Accidental(..), Quality(..),
              Pitch(..), Interval(..),
              Transpose(..),
              AbstractNote(..), Note1, Note2, Note3,
              AbstractPhrase(..),
              Degree(..), Ficta(..), noteToSound,
              apPitch,
              mapPhrase, absolute, normalise, faInt, faPitch, Music(..), mapMusic)

import Tuning
import FiveLimit (JustTuning(..), JustPitch(..), JustInt(..), ForceJustTuning(..))
import Scales (minor, major, HarmonicMinor(..), Minor, Major,
               harmonicminor, infiniteScale, chromaticScale)
import Shortcuts
import Lilypond
import Output


p = Pythagorean (pitch A Na, AbstractPitch3 440.0)

-- et = TET12 (pitch A Na, AbstractPitch3 440.0)

et = ArbitraryTET 62 (pitch A Na, AbstractPitch3 440.0)

qc = QCMeanTone (pitch A Na, AbstractPitch3 440.0)

me = Metronome 240


cnotes1 = [g, a, b, c, c, c, b, c]
cnotes2 = [e, f, d, e, d, e, d, e]
cnotes3 = map (.-^ _P8) [c, f, gis, a, fis, g, g, c]

chords = Voices $ map (\p -> phrase $ zipWith note p (repeat minim)) [cnotes1, cnotes2, cnotes3]

chordsounds = mapMusic (mapPhrase (noteToSound et me)) chords

str = show chordsounds

main = putStrLn str

