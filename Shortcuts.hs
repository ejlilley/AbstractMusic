module Shortcuts (pitch, freq, int, rhythm, dotted, ddotted, tdotted, tie, phrase, note,
                  crotchet, minim, semibreve, breve, long, quaver, semiquaver,
                  demisemiquaver, hemidemisemiquaver,
                  sharp, flat, natural,
                  aes, a, ais, bes, b, bis, ces, c, cis, des, d, dis, ees, e, eis, fes, f, fis, ges, g, gis,
                  d1, _P1, _A1, d2, m2, _M2, _A2, d3, m3, _M3, _A3, d4, _P4, _A4, d5, _P5, _A5, d6, m6, _M6, _A6, d7, m7, _M7, _A7, d8, _P8, _A8,
                  comma) where

import Prelude hiding ((^))
import Data.Ratio
import Music (AbstractPitch2(..), AbstractInt2(..), AbstractDur2(..),
              AbstractPitch3(..), AbstractInt3(..), AbstractDur3(..),
              AbstractPitch1(..), AbstractInt1(..), AbstractDur1(..),
              Name(..), Quality(..), Number(..), Accidental(..), Note2(..),
              AbstractNote(..), AbstractPhrase(..), Pitch(..), Interval(..),
              Transpose(..), Note(..), Duration(..))

import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace
import Data.Semigroup hiding (Min)


-------- handy shortcuts:

pitch :: Name -> Accidental -> AbstractPitch2
pitch n a = AbstractPitch2 n a

freq :: Double -> AbstractPitch3
freq f = AbstractPitch3 f

int :: Quality -> Number -> AbstractInt2
int q n = AbstractInt2 q n

rhythm a b = AbstractDur2 (a % b)

dotted (AbstractDur2 r) = AbstractDur2 (r * (3%2))
ddotted (AbstractDur2 r) = AbstractDur2 (r * (7%4))
tdotted (AbstractDur2 r) = AbstractDur2 (r * (15%8))

tie (AbstractDur2 r) (AbstractDur2 s) = AbstractDur2 (r + s)

(∧) p = p .+^ octave
(∨) p = p .-^ octave

phrase ns = AbstractPhrase ns
note p d = AbstractPitch p d

---- note durations

crotchet = rhythm 1 4
minim = rhythm 1 2
semibreve = rhythm 1 1
breve = rhythm 2 1
long = rhythm 4 1
quaver = rhythm 1 8
semiquaver = rhythm 1 16
demisemiquaver = rhythm 1 32
hemidemisemiquaver = rhythm 1 64

---- note pitches

sharp = Sh Na
flat = Fl Na
natural = Na

aes = pitch A flat
a = pitch A natural
ais = pitch A sharp
bes = pitch B flat
b = pitch B natural
bis = pitch B sharp
ces = pitch C flat
c = pitch C natural
cis = pitch C sharp
des = pitch D flat
d = pitch D natural
dis = pitch D sharp
ees = pitch E flat
e = pitch E natural
eis = pitch E sharp
fes = pitch F flat
f = pitch F natural
fis = pitch F sharp
ges = pitch G flat
g = pitch G natural
gis = pitch G sharp


---- intervals

comma = int (Dim Min) (Negative Second)

d1 = int (Dim Perf) Unison
_P1 = int Perf Unison
_A1 = int (Aug Perf) Unison
d2 = int (Dim Min) Second
m2 = int Min Second
_M2 = int Maj Second
_A2 = int (Aug Maj) Second
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

