\begin{code}
{-#
\end{code}

# John Bull -- Ut, re, mi, fa, sol, la

Fitzwilliam Virginal Book, No. 60

----------------

\begin{code}
#-}
\end{code}


\begin{code}
{-# LANGUAGE PostfixOperators,
             FlexibleContexts #-}

module Bull where

import Music (mapPhraseSingle, apPitch, apDur, apTran, apInt,
              explodeVoices, splitVoices, mapMusic, Metronome(..),
              AbstractNote(..), Music(..), mapPhrase, noteToSound, Tuning(..),
              revVoices)

import Shortcuts
import Output
import Csound.Patch
import qualified Data.Music.Lilypond as L
import Lilypond
import Tuning

-- import FiveLimit (JustTuning(..), ForceJustTuning(..))

downNoctaves n = mapPhraseSingle (apTran ((-n) *^ octave))

treble = downNoctaves 1 $ phrase $
  [Directive $ L.Clef L.Treble,
   Directive $ L.Time 4 2,
   note (g∨) s,
   note a s,
   note b s,
   note c s,
   note d s,
   note e s,
   note e s,
   note d s,
   note c s,
   rest m,
   note d s,
   note c s,
   note b m,
   rest s,
   rest s,
   rest m,
   note d m,
   note e m,
   note g m,
   note fis m,
   note g s,
   note (a∧) m,
   rest m,
   note d m,
   note cis m,
   note a m,
   note b m,
   note a m,
   rest m,
   note d m,
   note fis m,
   note e s,
   note d m,
   note (a∧) s,
   rest s,
   note d m,
   note e s,
   note fis s,
   note (b∧) (dotted m),
   note (a∧) cr,
   note gis m,
   rest m,
   note (b∧) s,
   note e s,
   note dis (dotted m),
   note cis cr,
   note b m,
   note cis m,
   note b m,
   rest s,
   rest s,
   rest m,
   note e (dotted m),
   note fis cr,
   note gis m,
   note e m,
   note ges s,
   note ees m,
   note (bes∧) m,
   note (aes∧) m,
   note ges s,
   note (aes∧) s,
   note (bes∧) s,
   note (bes∧) s,
   note (aes∧) s,
   note ges s,
   note f s,
   note ees s,
   note des s,
   rest s,
   note ees s,
   note f s,
   note g s,
   note (aes∧) s,
   note (bes∧) s,
   note (c∧) s,
   note (c∧) s,
   note (bes∧) s,
   note (aes∧) s,
   note g s,
   note f s,
   note ees s,
   rest s,
   note f s,
   note g s,
   note (a∧) s,
   note (bes∧) s,
   note (c∧) s,
   note (d∧) s,
   note (d∧) s,
   note (c∧) s,
   note (bes∧) s,
   note (a∧) s,
   note g s,
   note f s,
   rest m,
   note f m,
   note ees m,
   note (aes∧) m,
   note g m,
   note f s,
   note e m,
   note f m,
   note (bes∧) s,
   note g m,
   note (aes∧) m,
   note f (dotted m),
   note g cr,
   note (aes∧) m,
   note g s,
   rest s,
   rest m,
   note (ees∧) m,
   note (des∧) m,
   note (bes∧) m,
   note (c∧) (dotted m),
   note (bes∧) cr,
   note (aes∧) m,
   note ees m,
   note f s,
  rest s,
  rest m,
  note f m,
  note (bes∧) m,
  note g m,
  note (a∧) (dotted m),
  note (a∧) cr,
  note g s,
  rest m,
  note ees m,
  note (aes∧) m,
  note f m,
  note g m,
  note (c∧) m,
  note (f∧) m,
  note (d∧) m,
  note (ees∧) (dotted m),
  note (ees∧) cr,
  note (d∧) (dotted m),
  note (c∧) cr,
  note (bes∧) cr,
  note (c∧) cr,
  note (d∧) m,
  note (c∧) s,
  note (b∧) m,
  note (a∧) s,
  note gis m,
  note (a∧) s,
  rest s,
  rest m,
  note (c∧) cr,
  note (d∧) q,
  note (e∧) q,
  note (f∧) cr,
  note (e∧) (dotted cr),
  note (d∧) q,
  note (c∧) cr,
  note (b∧) cr,
  note (d∧) m,
  note (c∧) q,
  note (b∧) q,
  note (a∧) cr,
  note (c∧) m,
  note (b∧) q,
  note (a∧) q,
  note (g) (dotted cr),
  note (a∧) q,
  note (b∧) cr,
  note (c∧) cr,
  note (d∧) s,
  note (g) m,
  note (c∧) s,
  note (b∧) m,
  note (a∧) s,
  note (g) s,
  rest m,
  note (d∧) s,
  note (b∧) m,
  note (cis∧) cr,
  note (d∧) cr,
  note (e∧) m,
  note (d∧) m,
  note (b∧) (dotted m),
  note (cis∧) cr,
  note (d∧) cr,
  note (b∧) cr,
  note (cis∧) m,
  note (a∧) m,
  note (b∧) m,
  note (e∧) (dotted m),
  note (d∧) cr,
  note (d∧) s,
  note (cis∧) m,
  note (d∧) (dotted m),
  note (d∧) cr,
  note (cis∧) m,
  note (a∧) m,
  note (b∧) (dotted m),
  note (b∧) cr,
  note (a∧) s,
  rest m,
  note e m,
  note fis m,
  note (a∧) (dotted m),
  note fis cr,
  note gis m,
  note (a∧) s,
  rest s,
  note (b∧) (dotted m),
  note (cis∧) cr,
  note (d∧) cr,
  note (cis∧) cr,
  note (b∧) cr,
  note (a∧) cr,
  note (cis∧) m,
  note (b∧) (dotted m),
  note (a∧) cr,
  note fis m,
  note gis m,
  note (a∧) s,
  note gis m,
  note fis s,
  rest m,
  note e m,
  note fis (dotted m),
  note fis cr,
  note gis m,
  note (b∧) s,
  note (ais∧) m,
  note (b∧) s,
  rest s,
  rest s,
  rest m,
  note (dis∧) m,
  note (cis∧) m,
  note (ais∧) m,
  note (b∧) m,
  note gis m,
  note (a∧) (dotted m),
  note gis cr,
  note fis cr,
  note e cr,
  note fis s,
  note e m,
  note fis m,
  note e cr,
  note (a∧) m,
  note gis q,
  note fis q,
  note gis m,
  note (a∧) m,
  note (c∧) (dotted m),
  note (b∧) q,
  note (a∧) q,
  note (b∧) m,
  note (c∧) m,
  note (b∧) (dotted m),
  note (a∧) cr,
  note (a∧) (dotted m),
  note gis q,
  note fis q,
  note gis m,
  note (a∧) m,
  note g (dotted m),
  note (a∧) cr,
  note (b∧) cr,
  note g cr,
  note (a∧) cr,
  note g m,
  note fis cr,
  note g br
  ]
  
alto = downNoctaves 1 $ phrase $
       [
         rest s,
         rest s,
         rest m,
         Directive $ L.Clef L.Alto,
         note (g∨) m,
         note a m,
         note c s,
         note b m,
         note c cr,
         note (g∨) cr,
         note c (dotted m),
         note b cr,
         note (g∨) cr,
         note a cr,
         note b s,
         note a s,
         note b s,
         note a s,
         note (g∨) s,
         note (g∨) s,
         note a s,
         note b s,
         note cis s,
         note d s,
         note e s,
         note fis s,
         note fis s,
         note e s,
         note d s,
         note cis s,
         note b s,
         note a s,
         note a s,
         note b s,
         note cis s,
         note dis s,
         note e s,
         note fis s,
         note gis s,
         note gis s,
         note fis s,
         note e s,
         note dis s,
         note cis s,
         note b br,
         note des s,
         note ees s,
         note f s,
         rest m,
         note des (dotted m),
         note ees cr,
         note f cr,
         note des cr,
         note ges (dotted m),
         note ges cr,
         note f m,
         note ees s,
         note d m,
         note ees m,
         note bes cr,
         note c cr,
         note des cr,
         note c q,
         note bes q,
         note aes cr,
         note bes cr,
         note ces cr,
         note bes m,
         note aes cr,
         note bes s,
         note aes m,
         rest cr,
         note aes cr,
         note bes m,
         note c m,
         note aes cr,
         note bes cr,
         note c cr,
         note d cr,
         note ees m,
         note ees s,
         note f m,
         note bes m,
         note ees m,
         rest cr,
         note ees cr,
         note (aes∧) (dotted cr),
         note g q,
         note f cr,
         note ees cr,
         note (aes∧) cr,
         note g (dotted cr),
         note f q,
         note f m,
         note e cr,
         note f m,
         note c (dotted cr),
         note d q,
         note ees cr,
         note ees m,
         note bes cr,
         note des (dotted m),
         note aes cr,
         note c m,
         note bes m,
         note c (dotted m),
         note bes cr,
         note c m,
         note d m,
         note bes m,
         note ees m,
         note c m,
         note f (dotted m),
         note bes m,
         note ees m,
         note c m,
         note f m,
         note d cr,
         note f cr,
         note g cr,
         note f m,
         rest cr,
         note d cr,
         note f cr,
         note c cr,
         note f q,
         note g q,
         note (a∧) cr,
         note g cr,
         note f m,
         note e cr,
         note f cr,
         note f m,
         note c cr,
         note d m,
         note c cr,
         note bes cr,
         note a cr,
         note bes cr,
         note c m,
         note (f∨) cr,
         note (g∨) cr,
         note aes cr,
         note bes cr,
         note c s,
         note bes m,
         note des m,
         note c (dotted m),
         note bes cr,
         note aes m,
         note des m,
         note bes m,
         note ees s,
         note d cr,
         note c cr,
         note d cr,
         note ees m,
         note d cr,
         note ees s,
         rest m,
         note f m,
         note ees m,
         note c m, -- ces in source
         note des (dotted m),
         note des cr,
         note c m,
         note ees m,
         note f m,
         note ees s,
         note d m,
         note ees m,
         note c m,
         note f m,
         note d m,
         note bes m,
         note ees m,
         note c m,
         note f s,
         note ees m,
         note d m,
         note ees s,
         note d m,
         note ees m, -- in manuscript, down a third
         rest m,
         rest s,
         rest m,
         note c m,
         note f m,
         note d m,
         note ees m,
         note d cr,
         note g m,
         note f m,
         note e cr,
         note d m,
         note f m,
         note e (dotted m),
         note d cr,
         note c m,
         note f (dotted cr),
         note e q,
         note d cr,
         note e q,
         note f q,
         note g cr,
         note d cr,
         note fis (dotted cr),
         note g q,
         note (a∧) cr,
         note g (dotted cr),
         note (a∧) q,
         note g cr,
         note e cr,
         note fis cr,
         note g m,
         note d cr,
         note e cr,
         note f m,
         note c cr,
         note d cr,
         note e (dotted cr),
         note f q,
         note g cr,
         note f q,
         note e q,
         note f m,
         note d m,
         note e (dotted m),
         note d cr,
         note e cr,
         note fis cr,
         note g cr,
         note e cr,
         note fis cr,
         note g m,
         note fis cr,
         note g s,
         rest s,
         rest s,
         rest m,
         note (a∧) s,
         note gis cr,
         note fis cr,
         note gis cr,
         note (a∧) m,
         note gis cr,
         note (a∧) (dotted cr),
         note g sq,
         note fis sq,
         note e cr,
         note fis cr,
         note g m,
         note e m,
         note (a∧) m,
         note d m,
         note g m,
         note e m,
         note fis m,
         note d m,
         note e m,
         note fis m,
         note b m,
         note e( dotted m),
         note cis cr,
         note dis m,
         note e m,
         note b m,
         note d m,
         note e m,
         note d m,
         note e m,
         rest cr,
         note e m,
         note fis cr,
         note gis cr,
         note e cr,
         note fis (dotted m),
         note e cr,
         note e s,
         note dis m,
         note e (dotted m),
         note b cr,
         note cis m,
         note dis m,
         note e m,
         note cis m,
         note b s,
         rest m,
         note b s,
         note cis s,
         note dis m,
         note e m,
         note dis m,
         note fis s,
         rest m,
         note fis (dotted m),
         note gis cr,
         note (ais∧) cr,
         note fis cr,
         note gis (dotted m),
         note fis cr,
         note dis cr,
         note fis cr,
         note fis (dotted m),
         note cis cr,
         note fis (dotted m),
         note dis cr,
         note e m,
         note d m,
         note cis s,
         note a m,
         note b (dotted m),
         note cis cr,
         note d m,
         note cis m,
         note fis m,
         note e m,
         note e (dotted m),
         note fis cr,
         note g s,
         note e s,
         note e m,
         rest m,
         note d s,
         rest m,
         note e s,
         note d m,
         note e m,
         note d s,
         note c m,
         note b m,
         note a m
       ]

tenor = downNoctaves 2 $ phrase $
        [
          rest s,
          rest s,
          rest s,
          rest m,
          Directive $ L.Clef L.Alto,
          note g s,
          note f m,
          note e (dotted m),
          note d cr,
          note c cr,
          note d cr,
          note e cr,
          note c cr,
          note g cr,
          note d cr,
          note g (dotted m),
          note e cr,
          note fis m,
          note g m,
          note g (dotted m),
          note f cr,
          note f m,
          note e (dotted m),
          note d cr,
          note c cr,
          note d cr,
          note e (dotted m),
          note d cr,
          note e cr,
          note f cr,
          note g m,
          note fis m,
          note g (dotted m),
          note (a∧) cr,
          note (b∧) s,
          note g m,
          rest m,
          note (b∧) m,
          note (a∧) (dotted m),
          note gis cr,
          note fis cr,
          note e q,
          note fis q,
          note g m,
          note e m,
          note fis cr,
          note (a∧) m,
          note gis cr,
          note (a∧) (dotted m),
          note e cr,
          note g m,
          note fis cr,
          note e cr,
          note fis m,
          note e cr,
          note d cr,
          note e m,
          note fis s,
          note gis s,
          note (ais∧) m,
          note (b∧) (dotted m),
          note (a∧) cr,
          note gis cr,
          note (a∧) cr,
          note (b∧) cr,
          note gis cr,
          note (a∧) m,
          note (b∧) (dotted m),
          note (cis∧) cr,
          note (b∧) cr,
          note (a∧) cr,
          note gis cr,
          note (a∧) cr,
          note (b∧) (dotted m),
          note (a∧) cr,
          note fis m,
          note (a∧) m,
          note gis (dotted m),
          note (a∧) cr,
          note (b∧) (dotted m),
          note (a∧) cr,
          note (a∧) s,
          note gis cr,
          note fis cr,
          note gis s,
          note (a∧) m,
          note (bes∧) (dotted m),
          note (bes∧) cr,
          note (c∧) m,
          note (des∧) (dotted m),
          note (aes∧) cr,
          note (bes∧) m,
          note (bes∧) m,
          note (aes∧) s,
          note ges cr,
          note (aes∧) cr,
          note (bes∧) (dotted m),
          note f cr,
          note ges m,
          note (aes∧) (dotted m),
          note (bes∧) m,
          note (bes∧) q,
          note (aes∧) q,
          note ges cr,
          note (aes∧) cr,
          note (bes∧) m,
          note f m,
          note (aes∧) cr,
          note ges q,
          note f q,
          note ees m,
          note f cr,
          note des cr,
          note ges s,
          note f m,
          note ges m,
          note (aes∧) m,
          rest m,
          note (aes∧) m,
          note (bes∧) m,
          note (bes∧) m,
          note (ces∧) m,
          note (aes∧) (dotted m),
          note g q,
          note f q,
          note g m,
          note (aes∧) m,
          rest cr,
          note ees cr,
          note (aes∧) (dotted cr),
          note g q,
          note f cr,
          note ees cr,
          note (bes∧) cr,
          note (aes∧) cr,
          note (bes∧) m,
          note (c∧) m,
          note (aes∧) cr,
          note (aes∧) cr,
          note (bes∧) cr,
          note (c∧) cr,
          note g (dotted cr),
          note (aes∧) q,
          note (bes∧) cr,
          note (bes∧) cr,
          note f m,
          note g cr,
          note (aes∧) m,
          note g cr,
          note (aes∧) (dotted m),
          note g cr,
          note (aes∧) cr,
          note f m,
          note (bes∧) m,
          note g m,
          note (c∧) m,
          note (bes∧) cr,
          note (a∧) cr,
          note g cr,
          note f m,
          note g s,
          note (a∧) m,
          note (bes∧) s,
          rest cr,
          note (bes∧) cr,
          note f (dotted cr),
          note g q,
          note (a∧) (dotted cr),
          note (bes∧) q,
          note (c∧) m,
          note (d∧) m,
          note (bes∧) m,
          note (c∧) m,
          rest cr,
          note f m,
          note d cr,
          note e m,
          note f (dotted m),
          note ees cr,
          note des s,
          note c cr,
          note d cr,
          note ees cr,
          note f cr,
          note g cr,
          note (aes∧) cr,
          note (bes∧) m,
          note (aes∧) m,
          note g m,
          note f s,
          note g m,
          note (c∧) m,
          note (aes∧) (dotted s),
          note (aes∧) m,
          note (c∧) m,
          note (bes∧) (dotted m),
          note (aes∧) cr,
          note f m,
          note g m,
          note (aes∧) (dotted m),
          note f cr,
          note g m,
          note (aes∧) (dotted m),
          note g cr,
          note (aes∧) cr,
          note (bes∧) cr,
          note (c∧) m,
          note (bes∧) s,
          note g m,
          note (a∧) (dotted m),
          note f cr,
          note (bes∧) m,
          note g m,
          note (c∧) (dotted m),
          note (a∧) cr,
          note (d∧) (dotted m),
          note (c∧) cr,
          note (c∧) s,
          note (bes∧) m,
          note (c∧) m,
          note (bes∧) cr,
          note (a∧) cr,
          note (bes∧) m,
          note (c∧) (dotted m),
          note (bes∧) cr,
          note (bes∧) s,
          note (a∧) m,
          note (bes∧) (dotted m),
          note (a∧) cr,
          note g m,
          note (bes∧) m,
          note (a∧) m,
          note g m,
          note f cr,
          note g cr,
          note (a∧) cr,
          note (b∧) cr,
          note (c∧) cr,
          note (a∧) cr,
          note (b∧) m,
          note (a∧) cr,
          note (b∧) q,
          note (c∧) q,
          note (d∧) cr,
          note (a∧) cr,
          note (b∧) cr,
          note (c∧) m,
          note (b∧) cr,
          note (c∧) cr,
          note (d∧) q,
          note (e∧) q,
          note (f∧) cr,
          note (e∧) cr,
          note (c∧) m,
          rest cr,
          note (c∧) cr,
          note (e∧) cr,
          note (d∧) q,
          note (c∧) q,
          note (b∧) cr,
          note (c∧) cr,
          note (d∧) cr,
          note (c∧) q,
          note (b∧) q,
          note (a∧) cr,
          note (b∧) cr,
          note (c∧) cr,
          note (b∧) q,
          note (a∧) q,
          note g m,
          note (a∧) cr,
          note (c∧) m,
          note (b∧) cr,
          note (c∧) (dotted m),
          note (b∧) cr,
          note (a∧) m,
          note g m,
          note (d∧) m,
          note (a∧) m,
          note (c∧) m,
          note (b∧) m,
          note (a∧) s,
          note (b∧) m,
          note (e∧) s,
          note (cis∧) m,
          note (d∧) s,
          note (b∧) m,
          note (d∧) m,
          note (e∧) m,
          note (cis∧) m,
          note (d∧) m,
          note (c∧) cr,
          note (b∧) cr,
          note (a∧) (dotted m),
          note (b∧) cr,
          note (c∧) cr,
          note (b∧) cr,
          note (a∧) cr,
          note g cr,
          note (a∧) m,
          note (b∧) (dotted m),
          note (a∧) cr,
          note (a∧) s,
          note gis m,
          note (a∧) s,
          note (b∧) m,
          note e m,
          note d m,
          note cis m,
          note b m,
          note e m,
          note a m,
          note (a∧) m,
          note (cis∧) s,
          note (b∧) s,
          note (a∧) s,
          note gis s,
          note fis s,
          note e s,
          rest s,
          note fis s,
          note gis s,
          note (ais∧) s,
          note (b∧) s,
          note (cis∧) s,
          note (dis∧) s,
          note (dis∧) s,
          note (cis∧) s,
          note (b∧) s,
          note (ais∧) s,
          note gis s,
          note fis s,
          rest s,
          note g s,
          note (a∧) s,
          note (b∧) s,
          note (c∧) s,
          note (d∧) s,
          rest m,
          note g m,
          note a s,
          note (b∧) s,
          note (c∧) s,
          note (b∧) s,
          note (a∧) s,
          note g s,
          rest s
        ]

bass = downNoctaves 2 $ phrase $
       [
         rest m,
         Directive $ L.Clef L.Bass,
         note g s,
         note f m,
         note e (dotted m),
         note d cr,
         note c m,
         note e m,
         note d s,
         note c m,
         note a (dotted m),
         note b cr,
         note c m,
         note b m,
         note a cr,
         note (g∨) cr,
         note a s,
         note (g∨) cr,
         note a cr,
         note b cr,
         note c cr,
         note d m,
         note a m,
         note c m,
         note (g∨) m,
         note c (dotted m),
         note b cr,
         note a m,
         note a m,
         note e m,
         note a m,
         note e s,
         note d m,
         note (g∨) m,
         note c m,
         note a m,
         note d (dotted m),
         note c q,
         note b q,
         note a m,
         note d s,
         note cis m,
         note d m,
         note b m,
         note a s,
         rest m,
         note d s,
         note cis cr,
         note b cr,
         note cis m,
         note d (dotted m),
         note b cr,
         note e (dotted m),
         note cis cr,
         note fis m,
         note b s,
         rest m,
         note e s,
         note dis m,
         note e s,
         note e m,
         note b (dotted m),
         note cis cr,
         note dis m,
         note cis m,
         note e m,
         note b s,
         note cis (dotted m),
         note d cr,
         note e s,
         note e s,
         note (a∧) m,
         note ges (dotted m),
         note ges cr,
         note (aes∧) m,
         note des m,
         note des m,
         note ees m,
         note ges s,
         note f m,
         note ees s,
         note d m,
         note ees m,
         note c m,
         note f m,
         note ees m,
         note ees m,
         note bes cr,
         note c cr,
         note des m,
         note aes cr,
         note bes cr,
         note ces m,
         note bes m,
         note (ges∨) m,
         note des s,
         rest s,
         note f s,
         note ees m,
         note ees m,
         note (aes∧) m,
         note f m,
         note ees m,
         note ees m,
         note aes s,
         rest s,
         rest m,
         note g m,
         note f (dotted cr),
         note g q,
         note f m,
         note ees cr,
         note c (dotted cr),
         note des q,
         note ees cr,
         note des cr,
         note bes (dotted cr),
         note c q,
         note des cr,
         note ees s,
         note aes s,
         rest m,
         note bes m,
         note ees m,
         note c m,
         note f (dotted m),
         note ees cr,
         note d m,
         note ees s,
         note f m,
         note bes s,
         rest cr,
         note bes cr,
         note d cr,
         note bes cr,
         note f (dotted cr),
         note g q,
         note (a∧) cr,
         note f cr,
         note (bes∧) (dotted cr),
         note (a∧) q,
         note g m,
         note f m,
         note (f∨) m,
         note bes m,
         note c m,
         note (f∨) s,
         rest s,
         note aes s,
         note bes s,
         note c s,
         note des s,
         note ees s,
         note f s,
         note f s,
         note ees s,
         note des s,
         note c s,
         note bes s,
         note aes s,
         rest s,
         note bes s,
         note c s,
         note d s,
         note ees s,
         note f s,
         note g s,
         note g s,
         note f s,
         note ees s,
         note d s,
         note c s,
         note bes s,
         rest s,
         note c s,
         note d s,
         note e s,
         note f s,
         note g s,
         note (a∧) s,
         note (a∧) s,
         note g s,
         note f s,
         note e s,
         note d s,
         note c s,
         rest s,
         note d s,
         note e s,
         note fis s,
         note g s,
         note (a∧) s,
         note (b∧) s,
         note (b∧) s,
         note (a∧) s,
         note g s,
         note fis s,
         note e s,
         note d s,
         rest s,
         note e s,
         note fis s,
         note gis s,
         note (a∧) s,
         note (b∧) s,
         note (cis∧) s,
         note e m,
         note (a∧) s,
         note gis m,
         note fis s,
         note e m,
         note e m,
         note a m,
         note b m,
         note e m,
         note a m,
         note e m,
         note e s,
         note dis m,
         note e m,
         note cis m,
         note fis s,
         note e m,
         note gis m,
         note fis s,
         note b (dotted m),
         note cis cr,
         note dis cr,
         note b cr,
         note fis m,
         rest m,
         note e cr,
         note fis cr,
         note gis m,
         note dis m,
         note fis (dotted m),
         note fis cr,
         note b m,
         note cis m,
         note (fis∨) s,
         note a s,
         note (g∨) s,
         note (fis∨) m,
         note a m,
         note d m,
         note e m,
         note a m,
         note (a∧) m,
         note g s,
         note c m,
         note e m,
         note a cr,
         note a cr,
         note c m,
         note b (dotted m),
         note b cr,
         note a cr,
         note b cr,
         note c cr,
         note d cr,
         note e m,
         note b m,
         note c m,
         note d m,
         note b m,
         note e m,
         note d s
       ]

v1 = downNoctaves 1 $ phrase $
  [Directive $ L.Clef L.Treble,
   note g s]

v2 = downNoctaves 1 $ phrase $
  [Directive $ L.Clef L.Alto,
   note c s]

v3 = downNoctaves 1 $ phrase $
  [Directive $ L.Clef L.Tenor,
   note a s]

v4 = downNoctaves 1 $ phrase $
  [Directive $ L.Clef L.Bass,
   note g s]

tuning = equal (a, freq 440)
speed = Metronome 180

music = Voices [treble, alto, tenor, bass]

-- music = Voices [v1, v2, v3, v4]

performance t = mapMusic (mapPhrase (noteToSound t speed)) music




\end{code}

