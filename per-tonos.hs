module Main where

import LilyParse
import Text.Parsec.String
import LilyConvert

import System.IO
import Control.Monad
import System.Environment
import System.Exit
import System.Console.GetOpt    

import Music (mapMusic, mapPhrase, noteToSound, Metronome(..), apTran, Music(..), dropPhraseEnd)
import Tuning
import Shortcuts
import Output
import Csound.Patch

getVar :: String -> LilyFile -> Maybe LilyExpr
getVar s ((Assignment (LilyAssignment s' e)):xs) = if s == s'
                                                   then Just e
                                                   else getVar s xs
getVar s (x:xs) = getVar s xs
getVar _ [] = Nothing

doParse :: String -> IO LilyFile
doParse fileName = parseFromFile parseLily fileName >>= either report return
    where report err = do hPutStrLn stderr $ "Error: " ++ show err
                          exitFailure

upTone = mapPhrase (apTran _M2)

delay d p = (phrase [rest d]) <> p

main = do
  l <- doParse "/home/edward/projects/typesetting/bach/mo/canon5.ly"

  let f = expandVariables l
      thema = case (fmap (lilyExpr . expandExpr) $ getVar "thema" f) of (Just x) -> x
      canon = case (fmap (lilyExpr . expandExpr) $ getVar "canon" f) of (Just x) -> x
      n = 29
      -- n = 2
      lastNoteCanon = note (ees .-^ (2*^_P8) .+^ (n*^_M2)) minim
      lastNoteThema = note (c .-^ _P8 .+^ (n*^_M2)) minim
      themap = (foldl (<>) (phrase []) $ take n $ iterate upTone thema) <> (phrase [lastNoteThema])
      canonp1 = (dropPhraseEnd sq $ delay sq $ foldl (<>) (phrase []) $ take n $ iterate upTone canon) <> (phrase [lastNoteCanon])
      canonp2 = dropPhraseEnd (sq <> s) $ delay (s <> sq) $ mapPhrase (apTran _P5) $ foldl (<>) (phrase []) $ take n $ iterate upTone canon
      m = mapMusic (mapPhrase (noteToSound (qcmeantone (a, freq 220)) (Metronome 100))) $ Voices [themap, canonp1, canonp2]

  writeCsound "per-tonos.wav" harpsichord m
  -- playCsound harpsichord m
  exitWith ExitSuccess




