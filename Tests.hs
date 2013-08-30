module Main where

import Music (FreeAbelian(..), toInterval, intToFa, toPitch, pitchToFa, faInt, faPitch,
              AbstractPitch2(..), AbstractInt2(..), AbstractDur2(..))
import FiveLimit (FreeAbelian3(..), JustInt(..),
                  toIntJ, intJtoFa)
import Shortcuts

import Control.Exception
import Control.Monad
import Test.QuickCheck

import Data.AdditiveGroup
import Data.AffineSpace
import Data.VectorSpace
import Data.Semigroup hiding (Min)

instance Arbitrary FreeAbelian where
  arbitrary = liftM2 (::+) restricted restricted where
    restricted = choose ((-500), 500)

instance Arbitrary FreeAbelian3 where
  arbitrary = liftM3 (\a -> \b -> \c -> FA3 (a,b,c)) restricted restricted restricted where
    restricted = choose ((-500), 500)

instance Arbitrary AbstractInt2 where
  arbitrary = do a <- arbitrary
                 return (toInterval a)

instance Arbitrary AbstractPitch2 where
  arbitrary = do a <- arbitrary
                 return (toPitch a)

instance Arbitrary JustInt where
  arbitrary = do a <- arbitrary
                 return (toIntJ a)


-------- Interval tests

intervalAbelian a b = (a ^+^ b) == (b ^+^ a)

negateAdd a = (a ^-^ a) == zeroV

intervals :: Gen AbstractInt2
intervals = arbitrary

intervalsAreAbelian = forAll intervals intervalAbelian

intervalNegationGivesIdentity = forAll intervals negateAdd

intervalTests = (intervalsAreAbelian .&&. intervalNegationGivesIdentity)

-------- Pitch tests

pitches :: Gen AbstractPitch2
pitches = arbitrary

pitchAntiSim a b = (a .-. b) == (-1) *^ (b .-. a)

pitchIntAdd a b = (a .+^ (b .-. a)) == b

pitchDiffIsAntiSim = forAll pitches pitchAntiSim

pitchDiffCorrect = forAll pitches pitchIntAdd

pitchTests = pitchDiffIsAntiSim .&&. pitchDiffCorrect

-------- FreeAbelian internal tests

favalues :: Gen FreeAbelian
favalues = arbitrary

faPreservesPitch = forAll favalues (\f -> (pitchToFa . toPitch) f == f )
faPreservesIntervals = forAll favalues (\f -> (intToFa . toInterval) f == f )

faPreservesPitch' = forAll pitches (\f -> (toPitch . pitchToFa) f == f)
faPreservesIntervals' = forAll intervals (\f -> (toInterval . intToFa) f == f)

faTests = faPreservesPitch .&&. faPreservesIntervals .&&. faPreservesPitch' .&&. faPreservesIntervals'

-------- Tests for particular vales

testComma = (12 *^ _P5) ^-^ (7 *^ _P8) == comma

individualTests = testComma

-------- Just Int / Pitch / FreeAbelian 3 tests

fa3values :: Gen FreeAbelian3
fa3values = arbitrary

justIntervals :: Gen JustInt
justIntervals = arbitrary

fa3PreservesIntervals = forAll fa3values (\f -> (intJtoFa . toIntJ) f == f )

fa3PreservesIntervals' = forAll justIntervals (\f -> (toIntJ . intJtoFa) f == f)

fa3Tests = fa3PreservesIntervals .&&. fa3PreservesIntervals'


-------- Putting it all together

main = quickCheckResult (intervalTests .&&.
                         pitchTests .&&.
                         faTests .&&.
                         fa3Tests .&&.
                         individualTests)

