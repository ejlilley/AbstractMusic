module Algebra (intervalMod, intervalDiv, intervalDivisors, intervalDivisorsFrac) where

import Music (intToFa, toInterval, Interval(..), FreeAbelian(..), intervalPowerPositive)
import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace
import Data.Semigroup hiding (Min)
import Util (divides)
import Shortcuts

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

