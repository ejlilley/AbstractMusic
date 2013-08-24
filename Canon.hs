{-# LANGUAGE EmptyDataDecls, 
             MultiParamTypeClasses, 
             GADTs,
             RankNTypes,
             FlexibleInstances #-}

module Canon where

import Music (Name(..), Accidental(..), Scale(..), Tuning(..), Timing(..), Metronome(..), Duration(..),
              AbstractInt1(..), AbstractPitch1(..), AbstractDur1(..),
              AbstractInt2(..), AbstractPitch2(..), AbstractDur2(..),
              AbstractInt3(..), AbstractPitch3(..), AbstractDur3(..),
              Note(..),
              Name(..), Number(..), Accidental(..), Quality(..),
              Pitch(..), Interval(..),
              Transpose(..),
              AbstractNote(..), Note1, Note2, Note3,
              AbstractPhrase(..),
              Degree(..), Ficta(..), noteToSound,
              mapPhrase, absolute, normalise, repeatPhrase, foldPhrase, countDurs,
              explodeVoices, splitVoices, mapMusic,
              Music(..))

import Tuning
import Scales (minor, major, harmonicminor, melodicminor, infiniteScale)
import Shortcuts

import Output
import Lilypond

import Data.Ratio
import Data.Semigroup hiding (Min)
----

bassnotes = map (.-^ (2 *^ octave)) $ map (\d -> AbstractPitch1 d Neutral)
            [(DUp TO), DO, SM, ME, SD, TO, SD, DO]

p1notes = map (\d -> AbstractPitch1 d Neutral)
          [ME, ST, TO, (DDown LN), (DDown SM), (DDown DO), (DDown SM), (DDown LN)]

p2notes = map (.-^ octave) $ map (\d -> AbstractPitch1 d Neutral)
          [(DUp TO), LN, SM, DO, SD, ME, SD, ST]

p3notes = map (.-^ octave) $ map (\d -> AbstractPitch1 d Neutral)
          [TO, ME, DO, SD, ME, TO, ME, ST, TO, (DDown SM), TO, DO, SD, SM, DO, SD]

p4notes = map (.-^ octave) $ map (\d -> AbstractPitch1 d Neutral)
          [ME, TO, ST, LN, (DUp TO), (DUp ME), (DUp DO), DO, SM, SD, DO, ME, TO, (DUp TO), (DUp TO), LN]

p4rhythms = (take 14 $ repeat quaver) ++ [dotted quaver, semiquaver]

p5notes = map (.-^ octave) $ map (\d -> AbstractPitch1 d Neutral)
          [(DUp TO), LN, (DUp TO), TO, (DDown LN), DO, ST, ME, TO, (DUp TO), LN, SM, LN, (DUp ME), (DUp DO), (DUp SM), (DUp SD), (DUp ME), (DUp ST), (DUp SD),
           (DUp ME), (DUp ST), (DUp TO), LN, SM, DO, SD, ME, ST, SD, ME, ST]


-- key = major (pitch D Na)
key = minor (a .+^ d3 .+^ d3)

bass = AbstractPhrase (zipWith AbstractPitch
                       (map ((transpose (int Perf ((Negative (Compound Unison))))) . (applyScale key)) bassnotes)
                       (repeat crotchet))

cphrase p d k = AbstractPhrase (zipWith AbstractPitch (map (applyScale k) p) d)

p1 = cphrase p1notes (repeat crotchet) key
p2 = cphrase p2notes (repeat crotchet) key
p3 = cphrase p3notes (repeat quaver) key
p4 = cphrase p4notes p4rhythms key
p5 = cphrase p5notes (repeat semiquaver) key

v1 = (conn v4) <> p1 <> (conn v2) <> p2 <> p3 <> p4 <> p5 
v2 = p1 <> (conn v3) <> p2 <> p3 <> p4
v3 = p1 <> p2 <> p3 
v4 = repeatPhrase 5 bass

--et = Equal (a, freq 440)
et = Pythagorean (a, freq 440)

m = Metronome 120

canon = Start v1

voices = explodeVoices canon
