{-# LANGUAGE GADTs,
             FlexibleInstances,
             MultiParamTypeClasses #-}


module Keyboard where

import Music (AbstractPitch2(..), AbstractInt2(..), pitchToFa,
              FreeAbelian(..), Scale(..), AbstractPitch3(..))
import Tuning
import Scales (transposeScale, scaleDegree, completeScale,
               major, minor)
import Shortcuts
import Util (rotateN)

playKeyboard k t = (tune t) . (applyScale k)

faDot :: (Int, Int) -> AbstractPitch2 -> Int
faDot (x, y) p = let a ::+ d = pitchToFa p
                 in x*a + y*d

numbering7 = faDot (0, 1)
numbering12 = faDot (1, 0)
numbering19 = faDot (1, 1)

data Wolf = Wolf AbstractPitch2 deriving Show
wolfChromatic = [aes, a, bes, b, c, cis, d, ees, e, f, fis, g]

instance Scale Wolf AbstractPitch2 AbstractInt2 where
  tonic (Wolf t) = t
  scale s = let newScale = transposeScale wolfChromatic a (tonic s)
                rotated = rotateN (12 - (numbering12 (tonic s))) newScale
                --transposed = map (.-^ _P8) rotated
                transposed = rotated
            in take 11 transposed
  applyScale s p = let i = numbering12 p
                       notes = completeScale s i
                   in notes !! (abs i)

standardWolf = Wolf a


data Costeley = Costeley AbstractPitch2 deriving Show
costeleyScale = [aes, a, ais, bes, b, bis, c, cis, des, d, dis, ees, e, eis, f, fis, ges, g, gis]

instance Scale Costeley AbstractPitch2 AbstractInt2 where
  tonic (Costeley t) = t
  scale s = let newScale = transposeScale wolfChromatic a (tonic s)
                rotated = rotateN (19 - (numbering19 (tonic s))) newScale
                transposed = map (.+^ _P8) rotated
            in take 18 transposed
  applyScale s p = let i = numbering19 p
                       notes = completeScale s i
                   in notes !! (abs i)

