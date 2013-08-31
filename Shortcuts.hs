{-# LANGUAGE EmptyDataDecls, 
             MultiParamTypeClasses, 
             FunctionalDependencies,
             PostfixOperators,
             FlexibleContexts #-}


module Shortcuts (pitch, freq, rat, int, rhythm, dotted, ddotted, tdotted, tie, phrase, note, rest,
                  crotchet, minim, semibreve, breve, long, quaver, semiquaver, conn, co,
                  m, cr, br, s, q, sq,
                  demisemiquaver, hemidemisemiquaver,
                  sharp, flat, natural,
                  aeses, aes, a, ais, aisis, beses, bes, b, bis, bisis, ceses, ces, c, cis, cisis, deses, des, d, dis, disis,
                  eeses, ees, e, eis, eisis, feses, fes, f, fis, fisis, geses, ges, g, gis, gisis,
                  d1, _P1, _A1, d2, m2, _M2, _A2, d3, m3, _M3, _A3, d4, _P4, _A4, d5, _P5, _A5, d6, m6, _M6, _A6, d7, m7, _M7, _A7, d8, _P8, _A8,
                  comma, (.+^), (<>), (.-^), (.-.), (^-^), (^+^), (^*), (*^), Interval(..), Pitch(..), Duration(..),
                  (∨), (∧), (∨∨), (∧∧)) where

import Data.Ratio
import Music (AbstractPitch2(..), AbstractInt2(..), AbstractDur2(..),
              AbstractPitch3(..), AbstractInt3(..), AbstractDur3(..),
              AbstractPitch1(..), AbstractInt1(..), AbstractDur1(..),
              Name(..), Quality(..), Number(..), Accidental(..), Note2(..),
              AbstractNote(..), AbstractPhrase(..), Pitch(..), Interval(..),
              Transpose(..), Note(..), Duration(..), FreeAbelian(..),
              pitchToFa, intToFa, note, phrase, rest)
import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace
import Data.Semigroup hiding (Min)


-------- handy shortcuts:

pitch :: Name -> Accidental -> AbstractPitch2
pitch n a = AbstractPitch2 n a

freq :: Double -> AbstractPitch3
freq f = AbstractPitch3 f

rat :: Double -> AbstractInt3
rat f = AbstractInt3 f

int :: Quality -> Number -> AbstractInt2
int q n = AbstractInt2 q n

rhythm a b = AbstractDur2 (a % b)

conn c = AbstractPhrase [Conn c]

co c = Conn c

dotted (AbstractDur2 r) = AbstractDur2 (r * (3%2))
ddotted (AbstractDur2 r) = AbstractDur2 (r * (7%4))
tdotted (AbstractDur2 r) = AbstractDur2 (r * (15%8))

tie (AbstractDur2 r) (AbstractDur2 s) = AbstractDur2 (r + s)

(∧) p = p .+^ octave
(∧∧) p = p .+^ octave .+^ octave
(∨) p = p .-^ octave
(∨∨) p = p .-^ octave .-^ octave

(!) :: Int -> Int
(!) 1 = 1
(!) x = x * (!) (x - 1)
        
---- note durations

crotchet = rhythm 1 4
cr = crotchet
minim = rhythm 1 2
m = minim
semibreve = rhythm 1 1
s = semibreve
breve = rhythm 2 1
br = breve
long = rhythm 4 1
quaver = rhythm 1 8
q = quaver
semiquaver = rhythm 1 16
sq = semiquaver
demisemiquaver = rhythm 1 32
hemidemisemiquaver = rhythm 1 64

---- note pitches

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

