module MusicGraph where

import Music (Name(..), Accidental(..),
              Quality(..), Number(..), Ratio(..),
              sharp, flat, natural, pitch, int, note, relnote,
              crotchet, minim, semibreve, quaver,
              AbstractPitch2(..), AbstractInt2(..), AbstractDur2(..),
              AbstractNote(..), Note)


x = note (pitch A sharp) minim
y = note (pitch G flat) quaver

z = relnote (int Dim Fourth) crotchet
