\begin{code}
{-#
\end{code}

# Guillaume Costeley's 19TET

Audio file [here](costeley.ogg).

Rather crude score (Lilypond) [here](lily.html).

Note: obviously the input syntax is a bit shoddy...not least because it's
all absolute pitches, not relative. Designing something radically
different & better (e.g. parsing lilypond/PMX) would be a good
idea.

----------------

\begin{code}
#-}
\end{code}


\begin{code}
{-# LANGUAGE PostfixOperators,
             FlexibleContexts #-}

module Costeley where

import Music (mapPhraseSingle, apPitch, apDur, apTran, apInt,
              explodeVoices, splitVoices, mapMusic, Metronome(..),
              Music(..), mapPhrase, noteToSound, Tuning(..),
              revVoices)

import Shortcuts
import Output
import Lilypond
import Tuning
\end{code}
\begin{code}
downNoctaves n = mapPhraseSingle (apTran ((-n) *^ octave))

v4 = downNoctaves 2 $ phrase $
  [note (a∧) s,
   note (a∧) s,
   note d s,
   note (bes∧) (dotted s),
   note (a∧) m,
   co v3,
   note g s,
   rest m,
   note g m,
   note (bes∧) m,
   note (bes∧) m,
   note (c∧) m,
   note (c∧) m,
   note g s,
   rest m,
   note c m,
   note g m,
   note g m,
   note (aes∧) (dotted m),
   note g cr,
   note f m,
   note f m,
   note c s,
   rest s,
   rest br,
   rest s,
   rest m,
   note f m,
   note f s,
   note bes s,
   note ges (dotted s),
   note f m,
   note ees s,
   rest m,
   note bes m,
   note ges m,
   note ges m,
   note (aes∧) m,
   note (aes∧) m,
   note ees s,
   rest m,
   note aes m,
   note ees m,
   note ees m,
   note fes (dotted m),
   note ees cr,
   note des m,
   note des m,
   note aes s,
   rest m,
   note des m,
   note fes (dotted s),
   note ces m,
   note des m,
   note ais m,
   note ces s,
   note fes (dotted s),
   note fes m,
   note fes m,
   note fes m,
   note ges m,
   note dis m,
   note fes s]

v3 = downNoctaves 1 $ phrase $
  [note d s,
   note d s,
   note (g∨) s,
   note ees (dotted s),
   note d m,
   co v2,
   note c (dotted s),
   note b m,
   note ees m,
   note ees m,
   note f m,
   note f s,
   note e m,
   note f m,
   note (f∨) m,
   note c m,
   note c m,
   note des (dotted m),
   note c cr,
   note bes m,
   note bes m,
   note (f∨) s,
   rest m,
   note c m,
   note des (dotted m),
   note c cr,
   note bes m,
   note bes s,
   note (a∨) m,
   note bes s,
   note bes s,
   note (ees∨) s,
   note ces s,
   note bes s,
   note (aes∨) s,
   rest m,
   note (g∨) m,
   note ces m,
   note ces m,
   note des m,
   note des m,
   note c m,
   note c m,
   note des s,
   note ces s,
   note fes m,
   note ees m,
   note des s,
   note ces s,
   note aes s,
   rest m,
   note aes m,
   note aes m,
   note aes m,
   note ais m,
   note ais m,
   note aes s]
   
v2 = downNoctaves 1 $ phrase $
  [note g s,
   note g s,
   note c s,
   note (aes∧) (dotted s),
   note g m,
   co v1,
   note f s,
   rest m,
   note e m,
   note (aes∧) m,
   note (aes∧) m,
   note (bes∧) m,
   note (bes∧) s,
   note (a∧) cr,
   note g cr,
   note (a∧) cr,
   note g cr,
   note f cr,
   note ees cr,
   note f s,
   note des s,
   rest m,
   note f m,
   note ges m,
   note ges m,
   note f m,
   note f m,
   note ees (dotted br),
   note ees s,
   note ees s,
   note aes s,
   note fes s,
   note ees s,
   note des m,
   note fes m,
   note fes cr,
   note ees cr,
   note fes cr,
   note ges cr,
   note aes m,
   note ges (dotted m),
   note fes cr,
   note fes s,
   note ees m,
   note fes m,
   note ces m,
   note ces m,
   note ces m,
   note des (dotted s),
   note dis m,
   note ces s]

v1 = phrase $
  [note c s,
   note c s,
   note (f∨) s,
   note des (dotted s),
   note c cr,
   note bes cr,
   note c s,
   note bes s,
   rest m,
   note bes m,
   note des m,
   note des m,
   note ees m,
   note ees m,
   note d s,
   rest m,
   note bes m,
   note aes s,
   note (g∨) m,
   note (g∨) m,
   note ces m,
   note ces m,
   note bes m,
   note bes m,
   note aes br,
   rest m,
   note aes m,
   note aes (dotted s),
   note aes m,
   note ces (dotted m),
   note bes cr,
   note aes m,
   note ais m,
   note (ges∨) s,
   note (fes∨) (dotted br),
   rest s,
   co v1a]

v1a = phrase $
  [rest m,
   note (fes∨) m,
   note (fes∨) m,
   co v2a,
   co v3a,
   co v4a,
   note (fes∨) m,
   note aes s,
   note ais s,
   note (ges∨) m,
   note (ges∨) m,
   note (gis∨) (dotted m),
   note ais cr,
   note bis s,
   note ais s,
   note aes m,
   note aes m,
   note ais m,
   note ais s,
   note (gis∨) m,
   note (fis∨) s,
   note (eis∨) s]

v2a = downNoctaves 1 $ phrase $
  [note ces m,
   note fes (dotted m),
   note fes cr,
   note fes m,
   note ges m,
   note dis s,
   rest m,
   note dis m,
   note gis m,
   note gis m,
   note eis m,
   note eis m,
   note eis s,
   rest m,
   note eis m,
   note eis m,
   note eis m,
   note cis m,
   note cis m,
   note cis s]
   
v3a = downNoctaves 1 $ phrase $
  [note aes m,
   note ces (dotted m),
   note ces cr,
   note des m,
   note dis m,
   note ais m,
   note ais m,
   note bis (dotted m),
   note cis cr,
   note dis s,
   note cis s,
   note bis m,
   note bis m,
   note cis m,
   note cis s,
   note bis m,
   note ais s,
   note gis br]
   
v4a = downNoctaves 2 $ phrase $
  [note fes m,
   note fes m,
   note fes m,
   note (ais∧) m,
   note dis m,
   note dis m,
   note dis m,
   rest m,
   note gis m,
   note gis m,
   note gis m,
   note (ais∧) m,
   note (ais∧) m,
   note eis s,
   rest m,
   note ais m,
   note eis m,
   note eis m,
   note fis m,
   note fis m,
   note cis (dotted br)]

\end{code}


\begin{code}
-- tuning = Equal (a, freq 440)
tuning = TET19 (a, freq 440)
speed = Metronome 480

music = Start v4
voices = revVoices $ explodeVoices music

performance = mapMusic (mapPhrase (noteToSound tuning speed)) voices

play = playCsounds performance
\end{code}



