module Examples where


import Music (Name(..), Accidental(..), Scale(..), Tuning(..), Timing(..), Metronome(..),
              AbstractInt1(..), AbstractPitch1(..), AbstractDur1(..),
              AbstractInt2(..), AbstractPitch2(..), AbstractDur2(..),
              AbstractInt3(..), AbstractPitch3(..), AbstractDur3(..),
              Name(..), Number(..), Accidental(..), Quality(..),
              Pitch(..), Interval(..),
              Transpose(..),
              AbstractNote(..), Note1, Note2, Note3,
              AbstractPhrase(..), Phrase(..),
              Degree(..), Ficta(..), noteToSound,
              mapPhrase, absolute, normalise, faInt, faPitch)

import Tuning (Equal(..), Pythagorean(..), QCMeanTone(..))
import Scales (minor, major, HarmonicMinor(..), Minor, Major,
               harmonicminor, infiniteScale, chromaticScale)
import Shortcuts
import Lilypond
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

quavercmajscale = map (\n -> AbstractPitch n quaver) (scale cmajor)

-- example tuning systems:
p = Pythagorean (pitch A Na, AbstractPitch3 440.0)

et = Equal (pitch A Na, AbstractPitch3 440.0)

q = QCMeanTone (pitch A Na, AbstractPitch3 440.0)

m = Metronome 240

-- tuning some scales:
frequencies = map (tune et) (scale cmajor)
frequencies' = map (tune et) (scale csharpminor)


-- Play a tune

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

notes1 = AbstractPhrase $ zipWith AbstractPitch (map (applyScale cmajor) notes) (repeat crotchet)

notes2 = AbstractPhrase $ zipWith AbstractPitch (map (applyScale (harmonicminor (pitch D Na))) notes) (repeat crotchet)

