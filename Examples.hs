module Examples where


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
              apPitch, chord,
              mapPhrase, absolute, normalise, faInt, faPitch, Music(..), mapMusic)

import Tuning
import FiveLimit (JustTuning(..), JustPitch(..), JustInt(..), ForceJustTuning(..))
import Scales (minor, major, HarmonicMinor(..), Minor, Major,
               harmonicminor, infiniteScale, chromaticScale)
import Util (allPairs, interleave)
import Shortcuts
-- import LilyPrint
import Output

-- example notes:
fsharp = pitch F (Sh Na)
gsharp = pitch G (Sh Na)
gsharp' = pitch (Up G) (Sh Na)

n1 :: Note2
n1 = AbstractPitch fsharp quaver

n2 :: Note2
n2 = AbstractInt d5 quaver

n3 :: Note2
n3 = Rest quaver


somefreq = AbstractPitch3 4.52
somelength = AbstractDur3 4.56

n4 :: Note3
n4 = AbstractPitch somefreq somelength

-- example scales:
cmajor = major (pitch C Na)
bflatmajor = major (pitch B (Fl Na))
dminor = minor (pitch D Na)
csharpminor = minor (pitch C (Sh Na))
dsharpminor = harmonicminor (pitch D (Sh Na))

longquavercmajscale = phrase $ zipWith note (take 22 $ infiniteScale cmajor) (repeat quaver)
play_longquavercmajscale = playCsound $ mapPhrase (noteToSound et me) longquavercmajscale

-- example tuning systems:
p = pythagorean (pitch A Na, AbstractPitch3 440.0)

et = equal (pitch A Na, AbstractPitch3 440.0)

qc = qcmeantone (pitch A Na, AbstractPitch3 440.0)

me = Metronome 240

-- tuning some scales:
frequencies = map (tune et) (scale cmajor)
frequencies' = map (tune qc) (scale csharpminor)


---- Construct a tune from scale degrees (this is pretty unwieldy)

notes = [AbstractPitch1 TO Neutral,
         AbstractPitch1 DO Neutral,
         AbstractPitch1 ME Neutral,
         AbstractPitch1 TO Neutral,
         AbstractPitch1 (DDown LN) Neutral,
         AbstractPitch1 TO Neutral,
         AbstractPitch1 ST Neutral,
         AbstractPitch1 ME Neutral,
         AbstractPitch1 SD Neutral,
         AbstractPitch1 ME Neutral,
         AbstractPitch1 ST Neutral,
         AbstractPitch1 TO Neutral]
durs = [minim, minim, minim, minim, minim, crotchet, crotchet, tie minim quaver, quaver, quaver, quaver, crotchet]

notes1 = AbstractPhrase $ zipWith AbstractPitch (map (applyScale cmajor) notes) durs
notes2 = AbstractPhrase $ zipWith AbstractPitch (map (applyScale (harmonicminor (pitch D Na))) notes) durs


---- Some simple polyphony

cnotes1 = [g, a, b, c, c, c, b, c]
cnotes2 = [e, f, d, e, d, e, d, e]
cnotes3 = map (.-^ _P8) [c, f, gis, a, fis, g, g, c]

chords = Voices $ map (\p -> phrase $ zipWith note p (repeat minim)) [cnotes1, cnotes2, cnotes3]

-- note the arguments to noteToSound: 'et' is a tuning system, 'me' is a timing.
chordsounds = mapMusic (mapPhrase (noteToSound et me)) chords

-- and now to hear it through your speakers
playchordsounds = playCsounds chordsounds

----------------

pos = [0..]
neg = map (*(-1)) [1..]
ints = interleave pos neg
pairs = allPairs ints ints
intervals = map (\(a,d) -> a *^ _A1 ^+^ d *^ d2) pairs
