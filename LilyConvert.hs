{-# LANGUAGE FlexibleContexts,
             OverlappingInstances,
             TypeSynonymInstances,
             FlexibleInstances #-}


module LilyConvert (getScore, lilyExpr, expandExpr) where

import Prelude hiding (negate)

import Data.Ratio
import Data.List
import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad (MonadPlus(..), ap)

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Text.Parsec.Language (haskell) -- use Haskell-style floats etc. for now

import Music (AbstractNote(..),Note2(..), Name(..), Accidental(..),
              AbstractPitch2(..), AbstractDur2(..), AbstractInt2(..), AbstractPhrase(..),
              Note(..), Music(..), Pitch(..), Interval(..),
              explodeVoices, apRest, apDur, apTran,
              chord, countDurs, countDursRec, emptyPhrase, mapPhrase
             )
import Shortcuts

import LilyParse

-- import Debug.Trace

----------------

lilyDur :: LilyNoteDuration -> AbstractDur2
lilyDur d = AbstractDur2 d

lilyNoteName :: LilyNoteName -> (Name, Accidental)
lilyNoteName L_A = (A, Na)
lilyNoteName L_B = (B, Na)
lilyNoteName L_C = (C, Na)
lilyNoteName L_D = (D, Na)
lilyNoteName L_E = (E, Na)
lilyNoteName L_F = (F, Na)
lilyNoteName L_G = (G, Na)
lilyNoteName L_AIS = (A, Sh Na)
lilyNoteName L_BIS = (B, Sh Na)
lilyNoteName L_CIS = (C, Sh Na)
lilyNoteName L_DIS = (D, Sh Na)
lilyNoteName L_EIS = (E, Sh Na)
lilyNoteName L_FIS = (F, Sh Na)
lilyNoteName L_GIS = (G, Sh Na)
lilyNoteName L_AES = (A, Fl Na)
lilyNoteName L_BES = (B, Fl Na)
lilyNoteName L_CES = (C, Fl Na)
lilyNoteName L_DES = (D, Fl Na)
lilyNoteName L_EES = (E, Fl Na)
lilyNoteName L_FES = (F, Fl Na)
lilyNoteName L_GES = (G, Fl Na)
lilyNoteName L_AISIS = (A, Sh $ Sh Na)
lilyNoteName L_BISIS = (B, Sh $ Sh Na)
lilyNoteName L_CISIS = (C, Sh $ Sh Na)
lilyNoteName L_DISIS = (D, Sh $ Sh Na)
lilyNoteName L_EISIS = (E, Sh $ Sh Na)
lilyNoteName L_FISIS = (F, Sh $ Sh Na)
lilyNoteName L_GISIS = (G, Sh $ Sh Na)
lilyNoteName L_AESES = (A, Fl $ Fl Na)
lilyNoteName L_BESES = (B, Fl $ Fl Na)
lilyNoteName L_CESES = (C, Fl $ Fl Na)
lilyNoteName L_DESES = (D, Fl $ Fl Na)
lilyNoteName L_EESES = (E, Fl $ Fl Na)
lilyNoteName L_FESES = (F, Fl $ Fl Na)
lilyNoteName L_GESES = (G, Fl $ Fl Na)

isAorB x = elem x [L_A, L_AIS, L_AISIS, L_AES, L_AESES,
                   L_B, L_BIS, L_BISIS, L_BES, L_BESES]

lilyOctave :: LilyNote -> Int
lilyOctave (LilyNote n (LilyOctave o) _ _) = if isAorB n
                                                  then o - 1
                                                  else o - 2
lilyOctave (LilyNote n (LilyOctaveCheck o) d e) = lilyOctave (LilyNote n (LilyOctave o) d e)

lilyOctave' :: (LilyNoteName, LilyOctave) -> Int
lilyOctave' (n, LilyOctave o) = if isAorB n then o - 1 else o - 2
lilyOctave' (n, LilyOctaveCheck o) = lilyOctave' (n, LilyOctave o)

-- try: lilyNote $ LilyNote L_A (LilyOctave 2) (1 % 4) []
lilyNote :: LilyNote -> Note2
lilyNote p@(LilyNote n o d e) = let (n', a) = lilyNoteName n
                                    pitch = AbstractPitch2 n' a
                                    octaves = _P8 ^* (lilyOctave p)
                                in note (pitch .+^ octaves) (lilyDur d)
lilyNote (LilyRest d) = rest (lilyDur d)
lilyNote (LilyFullRest d) = rest (lilyDur d)
lilyNote (LilySkip d) = rest (lilyDur d)

lilySeq :: [LilyExpr] -> AbstractPhrase Note2
lilySeq s = foldl (<>) emptyPhrase $ map lilyExpr s

lilyPitch :: (LilyNoteName, LilyOctave) -> AbstractPitch2
lilyPitch (n, o) = let (n', a) = lilyNoteName n
                       pitch = AbstractPitch2 n' a
                       octaves = _P8 ^* (lilyOctave' (n,o))
                   in pitch .+^ octaves

justPitch (AbstractPitch p d) = p

-- setDur d = apDur (\_ -> d)

lilyChord :: LilyChord -> AbstractPhrase Note2
lilyChord (LilyChord c d) = chord (lilyDur d) (map (justPitch . lilyNote) c)

-- i.e. we are just ignoring grace notes for now
lilyGrace :: LilyGrace -> Note2
lilyGrace (BeforeGrace e n) = lilyNote n
lilyGrace (AfterGrace n e) = lilyNote n

singlet x = [x]

lilySim :: [LilyExpr] -> [AbstractPhrase Note2]
-- lilySim (LilySimultaneous (e:[])) = Conn $ phrase [lilyExpr e]
-- lilySim (LilySimultaneous (e:es)) = Conn $ phrase [lilySim (LilySimultaneous es), lilyExpr e]
-- lilySim (LilySimultaneous []) = Conn $ phrase []
lilySim s = map lilyExpr s

lilyExpr :: LilyExpr -> AbstractPhrase Note2 -- might always get multiple things back!
lilyExpr (Note n) = phrase [lilyNote n]
lilyExpr (Grace g) = phrase [lilyGrace g]
lilyExpr (Seq (LilySequential s)) = lilySeq s
lilyExpr (Sim (LilySimultaneous s)) = let phrases = lilySim s
                                          topPhrase = head phrases
                                          topLength = countDurs topPhrase
                                          otherPhrases = phrase $ map Conn (tail phrases)
                                          maxOtherLengths = foldl max zeroD $ map countDurs (tail phrases)
                                          -- Pad out the 'top' phrase by some rests if one of the daughter phrases is longer than it
                                          topPhrase' = if maxOtherLengths > topLength
                                                       then topPhrase <> (phrase $ singlet $ Rest $ subD maxOtherLengths topLength)
                                                       else topPhrase
                                      in otherPhrases <> topPhrase'
lilyExpr (Chord c) = lilyChord c
lilyExpr (Voice (LilyVoice _ _ e)) = lilyExpr e
lilyExpr (Staff (LilyStaff _ _ e)) = lilyExpr e
lilyExpr (Tie _) = emptyPhrase
lilyExpr (Tup (LilyTuplet r e)) = mapPhrase (apDur (\(AbstractDur2 d) -> AbstractDur2 (d*r))) $ lilyExpr e
lilyExpr (Trans (LilyTranspose (n,o) (n',o') e)) = let p = lilyPitch (n,o)
                                                       p' = lilyPitch (n',o')
                                                       i = p' .-. p
                                                   in mapPhrase (apTran i) $ lilyExpr e
lilyExpr _ = error "Cannot handle conversion of this kind of Lilypond expression!"

-- filter out types of Expr that we can't cope with

filterOut :: LilyExpr -> LilyExpr
filterOut (Seq (LilySequential s)) = Seq $ LilySequential (filterOutList s)
filterOut (Sim (LilySimultaneous s)) = Sim $ LilySimultaneous (filterOutList s)
filterOut (Voice (LilyVoice a b e)) = Voice $ LilyVoice a b (filterOut e)
filterOut (Staff (LilyStaff a b e)) = Staff $ LilyStaff a b (filterOut e)
filterOut (Rel (LilyRelative a e)) = Rel $ LilyRelative a (filterOut e)
filterOut (Fix (LilyFixed a e)) = Fix $ LilyFixed a (filterOut e)
filterOut (Trans (LilyTranspose a b e)) = Trans $ LilyTranspose a b (filterOut e)
filterOut (Lyrics (LilyLyrics a e)) = Lyrics $ LilyLyrics a (filterOut e)
filterOut e = e



filterOutList :: [LilyExpr] -> [LilyExpr]
filterOutList [] = []
-- filterOutList ((Tie _):es) = filterOutList es
filterOutList ((SlurExpr _):es) = filterOutList es
filterOutList ((Bar _):es) = filterOutList es
filterOutList ((Time _):es) = filterOutList es
filterOutList ((Key _):es) = filterOutList es
filterOutList ((Clef _):es) = filterOutList es
filterOutList ((Ident _):es) = filterOutList es
-- filterOutList ((Tup _):es) = filterOutList es
filterOutList ((Change _):es) = filterOutList es
-- filterOutList ((Rel _):es) = filterOutList es
-- filterOutList ((Fix _):es) = filterOutList es
filterOutList ((Lit _):es) = filterOutList es
filterOutList ((Assign _):es) = filterOutList es
filterOutList ((Lyrics _):es) = filterOutList es
filterOutList ((LyricMode _):es) = filterOutList es
filterOutList (LayoutExpr:es) = filterOutList es
filterOutList (MidiExpr:es) = filterOutList es
filterOutList (e:es) = (filterOut e) : (filterOutList es)

getScore :: LilyFile -> Music Note2
getScore ((Score e):xs) = Start $ lilyExpr (expandExpr e)
getScore (x:xs) = getScore xs
getScore [] = error "No score found in file"

expandExpr :: LilyExpr -> LilyExpr
expandExpr = (mapSeqs collapseTies) . expandDur . expandRel . filterOut

makeTies :: [LilyExpr] -> LilyExpr -> [LilyExpr] -> [LilyExpr]
-- makeTies p e n | trace (show p ++ ", " ++ show e ++ ", " ++ show n) False = undefined
-- makeTies [] e [] = [e]
makeTies p e [] = map (mapSeqs collapseTies) $ p ++ [e]
makeTies [] e n = makeTies [e] (head n) (tail n)
makeTies p e n = 
  let result = (makeTie (last p) e (head n))
  in if (length (tail n)) == 0
     then map (mapSeqs collapseTies) $ (init p) ++ result -- some of the 'notes' might actually be more Seqs/Sims etc. so recurse into them
     else case result of
           [] -> makeTies (init p) (head (tail n)) (tail (tail n))
           [e'] -> makeTies ((init p) ++ [e']) (head (tail n)) (tail (tail n))
           [e1,e2] -> makeTies ((init p) ++ [e1, e2]) (head (tail n)) (tail (tail n))
           [e1,e2,e3] -> makeTies ((init p) ++ [e1, e2]) e3 (tail n)

-- makeTies p e n = let result = (makeTie (last p) e (head n)) ++ (tail n)
                 -- in if (length result) < 3
                    -- then result
                    -- else let (p':e':n') = result
                         -- in makeTies ((init p) ++ [p',e']) (head n') (init n')

makeTie :: LilyExpr -> LilyExpr -> LilyExpr -> [LilyExpr]
makeTie (Note (LilyNote n1 o1 d1 e1)) (Tie LilyTie) (Note (LilyNote n2 o2 d2 e2)) =
  if (n1, o1) == (n2, o2)
  then [Note (LilyNote n1 o1 (d1 + d2) (e1 ++ e2))]
  else [(Note (LilyNote n1 o1 d1 e1)), (Note (LilyNote n2 o2 d2 e2))]
makeTie n1 n2 n3 = [n1, n2, n3]
-- [n1', n2', n3'] where
  -- n1' = mapSeqs collapseTies n1
  -- n2' = mapSeqs collapseTies n2
  -- n3' = mapSeqs collapseTies n3

collapseTies (LilySequential s) = LilySequential $ makeTies [] (head s) (tail s)

-- collapseTies (LilySequential s) = LilySequential $ collapseTies' s where
  -- collapseTies' [] = []
  -- collapseTies' ((Seq (LilySequential s)):es) = (Seq (LilySequential (collapseTies' s))):(collapseTies' es)
  -- collapseTies' (e1@(Note _):e2:e3:es) = let es' = (makeTie e1 e2 e3) ++ es
                                         -- in collapseTies' es'
  -- collapseTies' (e:es) = (mapSeqs collapseTies e) : (collapseTies' es)

