\begin{code}
{-#
\end{code}

# Lassus' Prophetiæ Sibyllarum -- Carmina Chromatico

Madrigal by Orlando de Lasso.

----------------

\begin{code}
#-}
\end{code}


\begin{code}
{-# LANGUAGE PostfixOperators,
             FlexibleContexts #-}

module Lassus where

import Music (mapPhraseSingle, apPitch, apDur, apTran, apInt,
              explodeVoices, splitVoices, mapMusic, Metronome(..),
              AbstractNote(..), Music(..), mapPhrase, noteToSound, Tuning(..),
              revVoices)

import Shortcuts
import Output
import qualified Music.Lilypond as L
import Lilypond
import Tuning

import FiveLimit (JustTuning(..), ForceJustTuning(..))

downNoctaves n = mapPhraseSingle (apTran ((-n) *^ octave))

bassus = downNoctaves 2 $ phrase $
  [Directive $ L.Clef L.Bass,
   rest s,
   note c (dotted s),
   note (g∨) m,
   note (g∨) s,
   note b br,
   note cis (dotted s),
   note e m,
   note e s,
   note fis s,
   note fis s,
   note g m,
   note c s,
   note f m,
   note bes s,
   note d m,
   note ees m,
   note d s,
   note g s,
   note c br,
   note e br,
   note a s,
   note d s,
   rest m,
   note g s,
   note g m,
   note e m,
   note e m,
   note fis s,
   note b s,
   note e s,
   note gis s,
   note (a∧) s,
   note d s,
   note g br,
   note c m,
   note c m,
   note f s,
   note d m,
   note g m,
   note ees (dotted m),
   note (bes∧) cr,
   note (bes∧) m,
   note f (dotted m),
   note (a∧) cr,
   note g m,
   note e m,
   note (a∧) (dotted m),
   note d cr,
   note g m,
   note f s,
   note bes br,
   note (f∨) m,
   note a m,
   note c (dotted m),
   note b cr,
   note a s,
   note (g∨) s]


tenor = downNoctaves 1 $ phrase $
  [Directive $ L.Clef L.Tenor,
   note (g∨) (dotted s),
   note (g∨) m,
   note (g∨) br,
   note (fis∨) br,
   note cis (dotted s),
   note b m,
   note b s,
   rest m,
   note a m,
   note a s,
   note b m,
   note c s,
   note a m,
   note bes s,
   note a m,
   note (g∨) m,
   note a s,
   note b s,
   note c br,
   note b br,
   note cis s,
   note d s,
   note (g∨) s,
   note b (dotted s),
   note b m,
   note ais m,
   note ais m,
   note b s,
   note b (dotted s),
   note b m,
   note cis s,
   note d m,
   note d s,
   note c (dotted m),
   note b q,
   note a q,
   note b m,
   note c m,
   note c m,
   note c s,
   note d m,
   note d m,
   note ees (dotted m),
   note d cr,
   note d m,
   note c (dotted m),
   note c cr,
   note d m,
   note e m,
   note cis (dotted m),
   note d cr,
   note bes m,
   note a s,
   note bes (dotted s),
   note bes m,
   note a m,
   note a (dotted m),
   note (g∨) cr,
   note (g∨) s,
   note (fis∨) m, -- editorial sharp
   note (g∨) s]
   

altus = downNoctaves 1 $ phrase $
  [Directive $ L.Clef L.Alto,
   rest s,
   note c (dotted s),
   note b m,
   note b s,
   note b br,
   note (gis∨) (dotted s),
   note e m,
   note e s,
   note cis s,
   note cis m,
   note d s,
   note e s,
   note c m,
   note d s,
   note d m,
   note c m,
   note d s,
   note d s,
   note g br,
   note e (dotted br),
   note fis s,
   note g s,
   note d s,
   note e s,
   note cis s,
   note dis s,
   note e s,
   note dis m,
   note e s,
   note a (dotted m),
   note (g∨) cr,
   note a m,
   note b m,
   note e m,
   note d s,
   note e m,
   note e m,
   note f s,
   note fis m,
   note g m,
   note g (dotted m),
   note f cr,
   note f m,
   note f (dotted m),
   note e cr,
   note g m,
   note gis m,
   note (a∧) (dotted m),
   note fis cr,
   note g m,
   note c s,
   note d br,
   note c m,
   note c m,
   note c br,
   note b s]
   
cantus = downNoctaves 1 $ phrase $
  [Directive $ L.Clef L.Treble,
   rest s,
   note e (dotted s),
   note d m,
   note d s,
   note dis br,
   note e (dotted s),
   note gis m,
   note gis s,
   note (a∧) s,
   note fis m,
   note (a∧) m,
   note g s,
   note g m,
   note f s,
   note f m,
   note fis m,
   note g (dotted m),
   note fis q,
   note e q,
   note fis m,
   note g s,
   note e br,
   note gis br,
   note (a∧) br,
   note (b∧) s,
   note g (dotted s),
   note g m,
   note fis m,
   note fis m,
   note fis s,
   note gis br,
   rest m,
   note e m,
   note fis s,
   note g m,
   note g m,
   note g s,
   note g m,
   note g m,
   note (a∧) s,
   note (a∧) m,
   note (bes∧) m,
   note (bes∧) (dotted m),
   note (bes∧) cr,
   note (bes∧) m,
   note (a∧) (dotted m),
   note (a∧) cr,
   note (b∧) m,
   note (b∧) m,
   note e (dotted m),
   note a cr,
   note d m,
   note f br,
   note f (dotted s),
   note e m,
   note e s,
   note (a∧) s,
   note d s]
   


tuning = TET12 (a, freq 440)
speed = Metronome 480

music = Voices [cantus, altus, tenor, bassus]

performance t = mapMusic (mapPhrase (noteToSound t speed)) music




\end{code}

