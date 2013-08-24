{-# LANGUAGE EmptyDataDecls, 
             OverloadedStrings,
             MultiParamTypeClasses, 
             UndecidableInstances, 
             IncoherentInstances, 
             DataKinds,
             FunctionalDependencies,
             FlexibleContexts,
             RankNTypes,
             OverlappingInstances,
             TypeSynonymInstances,
             ScopedTypeVariables,
             UnicodeSyntax,
             GADTSyntax,
             GADTs,
             TypeFamilies,
             ConstraintKinds,
             InstanceSigs,
             GeneralizedNewtypeDeriving,
             StandaloneDeriving,
             ViewPatterns,
             FlexibleInstances #-}

module Lilypond (lilypond, writeLilypond, lilypondFile, htmlFile) where

import Music (AbstractNote(..),Note2(..), Name(..), Accidental(..),
              AbstractPitch2(..), AbstractDur2(..), AbstractInt2(..), AbstractPhrase(..),
              Note(..), Music(..), Pitch(..), Interval(..), explodeVoices, apRest)
import Shortcuts


import Data.Ratio
import Data.Char

import qualified Music.Lilypond as L
import Text.Pretty hiding (int)

import System.IO
import System.Environment
import System.Posix.Temp
import System.Process
import GHC.IO.Exception

----

class Lily t where
  toLily :: t -> L.Music
  lilypond :: t -> Printer
  lilypond n = (pretty . toLily) n

instance Lily Note2 where
  toLily (AbstractPitch p d) = L.Note (toLPitch p) (Just (toLDur d)) []
  toLily r@(Rest _) = let rs = splitRest r
                          lrs = map toLRest rs
                      in L.Sequential lrs
  toLily (Directive d) = d                       

toLRest (Rest d) = L.Rest (Just (toLDur d)) []

toLPitch (AbstractPitch2 n a) = L.NotePitch (L.Pitch (toLName n, toLAcc a, toLOctave n)) Nothing

toLName A = L.A
toLName B = L.B
toLName C = L.C
toLName D = L.D
toLName E = L.E
toLName F = L.F
toLName G = L.G
toLName (Up n) = toLName n
toLName (Down n) = toLName n

toLAcc Na = 0
toLAcc (Sh a) = (toLAcc a) + 1
toLAcc (Fl a) = (toLAcc a) - 1

toLOctave (Up n) = (toLOctave n) + 1
toLOctave (Down n) = (toLOctave n) - 1
toLOctave A = 5
toLOctave B = 5
toLOctave _ = 6

toLDur (AbstractDur2 r) = L.Duration r

instance Lily (AbstractPhrase Note2) where
  toLily (AbstractPhrase ns) = L.Sequential (map toLily ns)

instance Lily (Music Note2) where
  toLily (Voices vs) = L.Simultaneous False (map toLily vs)
  toLily (Start v) = toLily (explodeVoices (Start v))



splitRest :: Note2 -> [Note2]
splitRest (Rest d) =
  map rest $ splitRest' [breve, semibreve, minim, crotchet, quaver, semiquaver] d
  where splitRest' [] r = [r]
        splitRest' ((AbstractDur2 c):cs) (AbstractDur2 r)
          | (r == c) = [AbstractDur2 c]
          | (r >= c) = (AbstractDur2 c) : (splitRest' ((AbstractDur2 c):cs) (AbstractDur2 (r - c)))
          | otherwise = splitRest' cs (AbstractDur2 r)

header = vcat ["\\version \"2.15.36\""]

layout = vcat $ [ "\\layout {",
                  "    \\override Staff.BarLine #'transparent = ##t",
                  "    \\context { \\Staff \\RemoveEmptyStaves }",
                  "}"]

snippet s = vcat $ ["\\clef alto",
                    "\\time 4/2",
                    s]

lilypondFile m = vcat [header, layout, (snippet (lilypond m))]


writeLilypond dir l = do (t,h) <- mkstemp (dir ++ "/lily.")
                         hPutStrLn h (runPrinter l)
                         putStrLn t

htmlFile s = vcat $
             [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">",
               "<!-- header_tag -->",
               "<HTML>",
               "<body>",
               "<p>",
               "<lilypond>",
               layout,
               lilypond s,
               "</lilypond>",
               "</p>",
               "</body>",
               "</html>"]
             

