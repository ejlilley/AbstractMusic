{-# LANGUAGE MultiParamTypeClasses, 
             RankNTypes,
             GADTs,
             FlexibleContexts,
             TypeSynonymInstances,
             ImpredicativeTypes,
             NoMonomorphismRestriction,
             FlexibleInstances #-}


-- module Polyphony (Cadence(..)) where
module Polyphony where

import Music (Note(..), AbstractInt2(..),
              AbstractDur2(..),
              AbstractNote(..), Note2,
              Music(..), explodeVoices, absolute, Duration(..),
              mapMusic, mapPhrase, apTran, AbstractPhrase(..),
              extractPitch)

import Shortcuts
import Data.AffineSpace
import Data.List
import Util (uniq, under)

-- import Data.Graph.Inductive.Graph


--     p3 --i3-> p4
--     ^         ^
--     |         |
--     i1        i2
--     |         |
--     p1 --i4-> p2

data Cadence n where
  Cadence :: (Note p i d, n ~ (AbstractNote p i d)) => (p -> d -> Music n) -> Cadence n

instance Eq (Cadence n) where
  (Cadence c1) == (Cadence c2) = (c1 middle unit) == (c2 middle unit)

instance Show (Cadence n) where
  show (Cadence c1) = let cad = show $ c1 middle unit
                      in "Cadence:" ++ cad

listNotes :: (Note p i d, n ~ (AbstractNote p i d)) => Music n -> [n]
-- the first note of each phrase
listNotes (Voices v) = map (\(AbstractPhrase p) -> head p) v

highestPitch :: (Note p i d, n ~ (AbstractNote p i d)) => [n] -> p
-- the highest pitch from a list of notes
highestPitch ns = head $ reverse $ sort $ map extractPitch ns

-- the highest note that begins a phrase
topPitch m = (highestPitch . listNotes) m

realiseCadence :: (Note p i d, n ~ (AbstractNote p i d)) => p -> d -> Cadence n -> Music n
-- realise cadence with specified base pitch
realiseCadence p d (Cadence c) = c p d

stackCadences :: (Note p i d, n ~ AbstractNote p i d) => Cadence n -> Cadence n -> Cadence n
stackCadences (Cadence c1) (Cadence c2) = Cadence $ \p d ->
  let c1' = c1 p d
      top = topPitch c1'
      c2' = c2 top d
  in c1' <> c2'

superimposeCadences :: (Note p i d, n ~ AbstractNote p i d) => Cadence n -> Cadence n -> Cadence n
superimposeCadences (Cadence c1) (Cadence c2) = Cadence $ \p d ->
  let c1' = c1 p d
      c2' = c2 p d
  in c1' <> c2'

makeFourNoteCadence :: (Note p i d, n ~ AbstractNote p i d) => (i,i,i) -> Cadence n
makeFourNoteCadence (i1,i2,i3) = Cadence $ \p d ->
  let n = note p d
      i4 = i1 ^+^ i3 ^-^ i2
      p1 = phrase [Conn p3, n, AbstractInt i4 d]
      p3 = phrase [AbstractInt i1 d, AbstractInt i3 d]
      m = Start (absolute p1)
  in explodeVoices m

-- see http://www.medieval.org/emfaq/harmony/2voice.html

-- m2-4   m2-5      M2-4   M2-5     m3-1   m3-5     M3-1   M3-5
-- ---   ----      ----   ----     ----   ----     ----   ----
-- c'-d'  f-g       b-c'   g-a      g-f    g-a      a-g    a-b
-- b -a   e-c       a-g    f-d      e-f    e-d      f-g    f-e
-- 
-- m6-8   m6-4      M6-8   M6-4     m7-5   M7-5
-- ---   ----      ----   ----     ----   ----
-- c'-d'  f'-e'     e'-f'  a'-g'    d'-c'  e'-d'
-- e -d   a -b      g -f   c'-d'    e -f   f -g

negM2 = _P1 ^-^ _M2
negm2 = _P1 ^-^ m2

directedCadences = map makeFourNoteCadence
                   [(m2, _P4, _M2),
                    (m2, _P5, _M2),
                    (_M2, _P4, m2),
                    (_M2, _P5, _M2),
                    (m3, _P1, negM2),
                    (m3, _P5, _M2),
                    (_M3, _P1, negM2),
                    (_M3, _P5, _M2),
                    (m6, _P8, _M2),
                    (m6, _P4, negm2),
                    (_M6, _P8, m2),
                    (_M6, _P4, negM2),
                    (m7, _P5, negM2),
                    (_M7, _P5, negM2)]
                   


makeObliqueCadence :: (Note p i d, n ~ AbstractNote p i d) => (i,[i]) -> Cadence n
makeObliqueCadence (i1,is) = Cadence $ \p d ->
  let n = note p d
      p1 = phrase [Conn p2, n] <> p1'
      p1' = phrase $ map (\_ -> AbstractInt unison d) is
      p2 = phrase [AbstractInt i1 d] <> p2'
      p2' = phrase $ map (`AbstractInt` d) is
      m = Start (absolute p1)
  in explodeVoices m




-- m2-1   M2-1      m3-1       m3-5         M3-1      M3-5
-- ---   ----     -------   ---------     -------   --------
-- f-e    a-g      f-(e)-d   c'-(d')-e'    a-(g)-f   b-(c')-d'
-- e -    g -      d     -   a       -     f     -   g      -
-- 
-- m6-5   M6-5     m7-8        m7-5        M7-8      M7-5
-- ---   ----     ----      ---------     ----    --------
-- f'-e'  d'-c'    c'-d'     f'-(e')-d'    e'-f'   e'-(d')-c'
-- a  -   f  -     d  -      g       -     f  -    f       -

obliqueResolving = map makeObliqueCadence
                   [(m2, [negm2]),
                    (_M2, [negM2]),
                    (m3, [negm2, negM2]),
                    (m3, [_M2, _M2]),
                    (_M3, [negM2, negM2]),
                    (_M3, [m2, _M2]),
                    (m6, [negm2]),
                    (_M6, [negM2]),
                    (m7, [_M2]),
                    (m7, [negm2, negM2]),
                    (_M7, [m2]),
                    (_M7, [negM2, negM2])]


-- Complete trine:
--      |g'
--      |  4
--     8|d'
--      |  5
--      |g

makeTriad :: (Note p i d, n ~ AbstractNote p i d) => (i,i) -> Cadence n
makeTriad (i1,i2) = Cadence $ \p d ->
  let n = note p d
      p1 = phrase [Conn p2, n]
      p2 = phrase [Conn p3, AbstractInt i1 d]
      p3 = phrase [AbstractInt i2 d]
      m = Start (absolute p1)
  in explodeVoices m

trine = makeTriad (_P5, _P4)
inverseTrine = makeTriad (_P4, _P5)



--     | d'           | e'
--     |   m3         |    M3
--   5 | b          5 | c'
--     |   M3         |    m3
--     | g            | a

quintaFissa = makeTriad (_M3, m3)
inverseQuintaFissa = makeTriad (m3, _M3)


--      | g'                | f'
--      |    5              |    4
--   M9 | c'             m7 | c'
--      |    5              |    4
--      | f                 | g

quintalFusion = makeTriad (_P5, _P5)
quartalFusion = makeTriad (_P4, _P4)


--     | d'                 | d'
--     |    M2              |    4
--   5 | c'               5 | a
--     |    4               |    M2
--     | g                  | g

splitFifth = makeTriad (_P4, _M2)
inverseSplitFifth = makeTriad (_M2, _P4)


--                                                 | f'    |
--                                                 |    m2 |
--      | c'                   | e'                | e'    | 4 |
--      |   4                  |    M2         m6  |    M3 |   |
--   m6 | g                 M6 | d'                | c'    |   | 5
--      |   m3                 |    5              |    m3     |
--      | e                    | g                 | a         |
-- 
--     m6/m3                  M6/5                   m6/5/m3
-- (m6 + m3 + 4)         (M6 + 5 + M2)     (m6 + 5 + m2 + m3 + M3 + 4)
-- 
--      | f'                   | e'
--      |   d5                 |    M3
--   m6 | b                 M6 | c'
--      |   M2                 |    4
--      | a                    | g
-- 
--    m6/M2                  M6/4
-- (m6 + M2 + d5)        (M6 + 4 + M3)


makeComb :: (Note p i d, n ~ AbstractNote p i d) => [i] -> Cadence n
makeComb ints = Cadence $ \p d ->
  let n = note p d
      ph = phrase [Conn $ (makeComb' ints), n]
      m = Start (absolute ph)
      makeComb' (i:[]) = phrase [AbstractInt i d]
      makeComb' (i:is) = phrase [Conn (makeComb' is), AbstractInt i d]
  in explodeVoices m


sixthCombinations = map makeComb
                    [[m3, _P4],
                     [_P5, _M2],
                     [m3, _M3, m2],
                     [_M2, d5],
                     [_P4, _M3]]

--                                                | d'   |
--                                                |   m3 |
--         | e'               | a'                | b    | 5 |
--         |   5              |   M3           m7 |   M3 |   |
--      M7 | a             m7 | f'                | g    |   | 5
--         |   M3             |   d5              |   m3     |
--         | f                | b                 | e        |


seventhCombinations = map makeComb
                      [[_M3, _P5],
                       [d5, _M3],
                       [m3, _M3, m3]]


cad1 = makeFourNoteCadence (_M3, _P1, negM2)
cad2 = makeFourNoteCadence (m3, _P5, _M2)
cad3 = stackCadences cad1 cad2


