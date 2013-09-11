{-# LANGUAGE TypeFamilies,
             MultiParamTypeClasses #-}

module Algebra (intervalMod, intervalDiv, intervalDivisors, intervalDivisorsFrac) where

import Music (intToFa, toInterval, Interval(..), FreeAbelian(..), intervalPowerPositive,
              Name(..), Accidental(..), Number(..), Quality(..), AbstractInt2(..),
              AbstractPitch2(..), faInt, toPitch, faPitch, pitchToFa, Transpose(..))
import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace
import Data.Semigroup hiding (Min)
import Util (divides, under)
import Shortcuts

-- Some functions that have a more mathematical focus.

intervalMod i di
  | (i > unison) = intervalModPos i di
  | (i < unison) = intervalModNeg i di
  | otherwise = unison
  where
    intervalModPos i di
      | (i < unison) = undefined
      | (i ^-^ di) < unison = i
      | otherwise = intervalMod (i ^-^ di) di
    intervalModNeg i di
      | (i > unison) = undefined
      | (i ^+^ di) > unison = i
      | otherwise = intervalMod (i ^+^ di) di

intervalDiv i di
  | (i > unison) = intervalDivPos i di
  | (i < unison) = intervalDivNeg i di
  | otherwise = 0 :: Int
  where 
    intervalDivPos i di
      | (i < unison) = undefined
      | (i ^-^ di) < unison = 0
      | otherwise = 1 + (intervalDiv (i ^-^ di) di)
    intervalDivNeg i di
      | (i > unison) = undefined
      | (i ^+^ di) > unison = 0
      | otherwise = 1 + (intervalDiv (i ^+^ di) di)

-- we want x,y where i = x*j + y*k
intervalDivisors i j k
  | (p == 0) = Nothing
  | not $ p `divides` r = Nothing
  | not $ p `divides` q = Nothing
  | otherwise = Just (r `div` p, q `div` p)
  where (m ::+ n) = intToFa i
        (a ::+ b) = intToFa j
        (c ::+ d) = intToFa k
        p = (a*d - b*c)
        q = (a*n - b*m)
        r = (d*m - c*n)
-- e.g., intervalDivisors comma _P5 _P8 = Just (12,-7), as expected.

-- todo: make a similar function for five-limit tuning (i.e. inverting a 3x3 matrix)

intervalDivisorsFrac i j k
  | (p == 0) = Nothing
  -- | not $ p `divides` r = Nothing
  -- | not $ p `divides` q = Nothing
  | otherwise = Just (r / p, q / p)
  where (m ::+ n) = intToFa i
        (a ::+ b) = intToFa j
        (c ::+ d) = intToFa k
        p = fromIntegral $ (a*d - b*c)
        q = fromIntegral $ (a*n - b*m)
        r = fromIntegral $ (d*m - c*n)



-- Can handle negative powers, if they exist.
intervalPower k i
  | (k >= 0) = Just $ intervalPowerPositive k i
  | not $ k' `divides` m = Nothing
  | not $ k' `divides` n = Nothing
  | otherwise = Just $ toInterval ( (m `div` k') ::+ (n `div` k') )
  where (m ::+ n) = intToFa i
        k' = -k



-- IntervalClass = Z + Z/(7*Z), wheras AbstractInt2 = Z + Z
-- (here '+' is direct sum)
--
-- \textsf{IntervalClass} = Z \oplus \frac{Z}{7Z}
--
-- \textsf{AbstractInt}_2 = Z \oplus Z
--
-- ...and likewise for PitchClass

data PitchClass = PC Name Accidental

data IntervalClass = IC Quality Number

instance Interval IntervalClass where
  add (IC q n) (IC p m) = toIC $ toInterval $ (faInt q n) + (faInt p m)
  sub (IC q n) (IC p m) = toIC $ toInterval $ (faInt q n) - (faInt p m)
  augment (IC q n) = toIC $ toInterval $ (faInt q n) + (1 ::+ 0)
  diminish (IC q n) = toIC $ toInterval $ (faInt q n) - (1 ::+ 0)
  grow (IC q n) = toIC $ toInterval $ (faInt q n) + (1 ::+ 1)
  shrink (IC q n) = toIC $ toInterval $ (faInt q n) - (1 ::+ 1)
  unison = toIC _P1
  octave = unison

instance Eq IntervalClass where
  (==) = (==) `under` (intToFa . icToInt)

instance Show IntervalClass where
  show i = "class:" ++ (show (icToInt i))

instance VectorSpace IntervalClass where
  type Scalar IntervalClass = Int
  a *^ b = toIC $ intervalPowerPositive a (icToInt b)


toIC i@(AbstractInt2 q (Negative n)) = let (IC q' n') = toIC (i ^+^ _P8)
                                       in IC q' (Negative n')
toIC i@(AbstractInt2 q (Compound n)) = toIC (i ^-^ _P8)
toIC (AbstractInt2 q n) = IC q n

icToInt (IC q n) = AbstractInt2 q n

instance Pitch PitchClass where
  sharpen (PC n a) = toPC $ toPitch $ (faPitch n a) + (1 ::+ 0)
  flatten (PC n a) = toPC $ toPitch $ (faPitch n a) - (1 ::+ 0)
  incr (PC n a) = PC (succ n) a
  decr (PC n a) = PC (pred n) a
  middle = PC A Na

instance Eq PitchClass where
  (==) = (==) `under` (pitchToFa . pcToPitch)

instance Show PitchClass where
  show p = "class:" ++ (show (pcToPitch p))

instance Ord PitchClass where
  compare = compare `under` (\(PC n _) -> fromEnum n)

instance Transpose PitchClass IntervalClass where
  transpose i p = toPC $ transpose (icToInt i) (pcToPitch p)
  interval p p' = toIC $ interval (pcToPitch p) (pcToPitch p')
  normalise _ _ n = n -- already normalised


instance AffineSpace PitchClass where
  type Diff PitchClass = IntervalClass
  (.-.) = flip interval
  (.+^) = flip transpose


toPC (AbstractPitch2 (Up n) a) = toPC (AbstractPitch2 n a)
toPC (AbstractPitch2 (Down n) a) = toPC (AbstractPitch2 n a)
toPC (AbstractPitch2 n a) = PC n a

pcToPitch (PC n a) = AbstractPitch2 n a
