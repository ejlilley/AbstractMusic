{-# LANGUAGE EmptyDataDecls, 
             MultiParamTypeClasses, 
             GADTs,
             RankNTypes,
             FlexibleInstances #-}

----------------
---------- Old, deprecated Csound interface!
---------- See CsoundExp.hs for better interface (incl. realtime support)
----------------

module Csound where

import Music (Note3, AbstractNote(..), AbstractPitch3(..), AbstractInt3(..), AbstractDur3(..), absolute, AbstractPhrase(..), Note(..), Music(..), explodeVoices)
import Util (log2)

import System.Process
import GHC.IO.Exception

-- import Codec.Midi

-- type MidiPitch = Double

-- freqToMidi :: Freq -> MidiPitch
-- freqToMidi f = 69 + 12*(log2 (f / 440))

-- midiToFreq :: MidiPitch -> Freq
-- midiToFreq d = (440*) $ 2 ** ((d - 69)/12)

csoundHeader = "f1  0   8192  10   1 .02 .01\n\nt 0 30"

testFreqList [] _ = ""
testFreqList (f:fs) n = "i1   " ++ (show n) ++ "    1    200      " ++ f ++ "\n\n"
                        ++ (testFreqList fs (n+1))

csoundFreqs freqs = csoundHeader ++ "\n"
                    ++ (testFreqList (map (\(AbstractPitch3 f) -> show f) freqs) 0)

csoundLine :: Double -> Double -> Double -> String
csoundLine time dur pitch = "i1   " ++ (show time) ++ "    " ++ (show dur) ++ "    200      " ++ (show pitch) ++ "\n\n"

phraseToList :: (Note p i d) => AbstractPhrase (AbstractNote p i d) -> [AbstractNote p i d]
phraseToList (AbstractPhrase ns) = ns

csound :: AbstractPhrase Note3 -> String
csound notes = csoundHeader ++ "\n" ++ csoundPhrase 0 notes'
--  where notes' = phraseToList $ absolute notes -- absolute is currently buggy
  where notes' = phraseToList notes

csounds :: [AbstractPhrase Note3] -> String
csounds notes = csoundHeader ++ "\n" ++ foldl (++) "" (map (csoundPhrase 0) notes')
--  where notes' = map (phraseToList . absolute) notes -- absolute is currently buggy
  where notes' = map phraseToList notes


csoundPhrase _ [] = ""
csoundPhrase n ((AbstractPitch (AbstractPitch3 p) (AbstractDur3 d)):notes) = (csoundLine n (d/1000) p) ++ (csoundPhrase (n + (d/1000)) notes)
csoundPhrase n ((Rest (AbstractDur3 d)):notes) = (csoundLine n (d/1000) 0) ++ (csoundPhrase (n + (d/1000)) notes)
csoundPhrase n (d:notes) = csoundPhrase n notes





testFreqs freqs = createProcess (proc "tones" (map (\(AbstractPitch3 f) -> show f) freqs))

playCsound notes = do writeFile "./csound-test.sco" $ csound notes;
                      readProcess "csound" ["-d", "csound-test/organ.orc", "csound-test.sco"] "";
                      system "mplayer -volume 48 test.wav"

playCsounds :: Music Note3 -> IO ExitCode
playCsounds (Start phrase) = playCsounds $ explodeVoices (Start phrase)
playCsounds (Voices phrases) = do writeFile "./csound-test.sco" $ csounds phrases;
                                  readProcess "csound" ["-d", "csound-test/organ.orc", "csound-test.sco"] "";
                                  system "mplayer test.wav"


