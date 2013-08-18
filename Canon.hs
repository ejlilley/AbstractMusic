module Canon where

import Music (Name(..), Accidental(..), Scale(..), Tuning(..), Timing(..), Metronome(..),
              AbstractInt1(..), AbstractPitch1(..), AbstractDur1(..),
              AbstractInt2(..), AbstractPitch2(..), AbstractDur2(..),
              AbstractInt3(..), AbstractPitch3(..), AbstractDur3(..),
              Name(..), Number(..), Accidental(..), Quality(..),
              Pitch(..), Interval(..),
              Transpose(..),
              AbstractNote(..), Note1, Note2, Note3,
              AbstractPhrase(..), Phrase(..),
              pitch, int, rhythm, sharp, flat, Degree(..), Ficta(..), freq, noteToSound,
              crotchet, minim, semibreve, quaver, semiquaver, dotted, tie,
              mapPhrase, absolute, normalise, concatPhrase, nullPhrase, freq)

import Tuning (Equal(..), Pythagorean(..), QCMeanTone(..))
import Scales (minor, major, HarmonicMinor(..), Minor, Major, harmonicminor, infiniteScale)

import Output (csoundFreqs, playCsound, playCsounds)

----

bassnotes = map (\d -> AbstractPitch1 d Neutral)
            [(DUp TO), DO, SM, ME, SD, TO, SD, DO]

p1notes = map (\d -> AbstractPitch1 d Neutral)
          [ME, ST, TO, (DDown LN), (DDown SM), (DDown DO), (DDown SM), (DDown LN)]

p2notes = map (\d -> AbstractPitch1 d Neutral)
          [(DUp TO), LN, SM, DO, SD, ME, SD, ST]

p3notes = map (\d -> AbstractPitch1 d Neutral)
          [TO, ME, DO, SD, ME, TO, ME, ST, TO, (DDown SM), TO, DO, SD, SM, DO, SD]

p4notes = map (\d -> AbstractPitch1 d Neutral)
          [ME, TO, ST, LN, (DUp TO), (DUp ME), (DUp DO), DO, SM, SD, DO, ME, TO, (DUp TO), (DUp TO), LN]

p4rhythms = (take 14 $ repeat quaver) ++ [dotted quaver, semiquaver]

p5notes = map (\d -> AbstractPitch1 d Neutral)
          [(DUp TO), LN, (DUp TO), TO, (DDown LN), DO, ST, ME, TO, (DUp TO), LN, SM, LN, (DUp ME), (DUp DO), (DUp SM), (DUp SD), (DUp ME), (DUp ST), (DUp SD),
           (DUp ME), (DUp ST), (DUp TO), LN, SM, DO, SD, ME, ST, SD, ME, ST]


key = major (pitch D Na)

bass = AbstractPhrase (zipWith AbstractPitch
                       (map ((transpose (int Perf ((Negative . Compound . Compound) (Compound Unison)))) . (applyScale key)) bassnotes)
                       (repeat crotchet))

rests = AbstractPhrase [Rest semibreve, Rest semibreve]

phrase p d k = AbstractPhrase (zipWith AbstractPitch (map (applyScale k) p) d)

p1 = phrase p1notes (repeat crotchet) key
p2 = phrase p2notes (repeat crotchet) key
p3 = phrase p3notes (repeat quaver) key
p4 = phrase p4notes p4rhythms key
p5 = phrase p5notes (repeat semiquaver) key

v1 = [rests, p1, p2, p3, p4, p5]
v2 = rests:v1
v3 = rests:v2
v4 = take (length v1) $ (repeat bass)

phrases t = map (mapPhrase (noteToSound t (Metronome 120))) (map (foldl concatPhrase nullPhrase) [v1,v2,v3,v4])
