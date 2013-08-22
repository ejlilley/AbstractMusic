module Mensuration where

import Data.Semigroup
import Shortcuts
import Music (Mensuration(..), MDur(..), AbstractDur1(..), AbstractDur2(..))


sgfold n x = foldl (<>) x $ take (n - 1) (repeat x)

convertDur :: AbstractDur1 -> AbstractDur2
convertDur (AbstractDur1 Mx) = breve
convertDur (AbstractDur1 Lo) = semibreve
convertDur (AbstractDur1 Br) = minim
convertDur (AbstractDur1 Sb) = crotchet
convertDur (AbstractDur1 Mi) = quaver
convertDur (AbstractDur1 Sm) = semiquaver
convertDur (AbstractDur1 Ff) = demisemiquaver
convertDur (AbstractDur1 Sf) = hemidemisemiquaver

data Tempus = Tempus Int Int
-- prolation of Breve, Semibreve

instance Mensuration Tempus where
  mensurate m (AbstractDur1 (Punctus n)) = dotted $ mensurate m (AbstractDur1 n)
  mensurate tp@(Tempus t p) (AbstractDur1 Mx) = sgfold 2 $ mensurate tp (AbstractDur1 Lo)
  mensurate tp@(Tempus t p) (AbstractDur1 Lo) = sgfold 2 $ mensurate tp (AbstractDur1 Br)
  mensurate tp@(Tempus t p) (AbstractDur1 Br) = sgfold t $ mensurate tp (AbstractDur1 Sb)
  mensurate tp@(Tempus t p) (AbstractDur1 Sb) = sgfold p $ mensurate tp (AbstractDur1 Mi)
  mensurate _ n = convertDur n
  






data Modus = Modus Int Int Int Int
-- prolation of Maxima, Longus, Breve, Semibreve

instance Mensuration Modus where
  mensurate m (AbstractDur1 (Punctus n)) = dotted $ mensurate m (AbstractDur1 n)
  mensurate mds@(Modus m l t p) (AbstractDur1 Mx) = sgfold m $ mensurate mds (AbstractDur1 Lo)
  mensurate mds@(Modus m l t p) (AbstractDur1 Lo) = sgfold l $ mensurate mds (AbstractDur1 Br)
  mensurate mds@(Modus m l t p) (AbstractDur1 Br) = sgfold t $ mensurate mds (AbstractDur1 Sb)
  mensurate mds@(Modus m l t p) (AbstractDur1 Sb) = sgfold p $ mensurate mds (AbstractDur1 Mi)
  mensurate _ n = convertDur n







