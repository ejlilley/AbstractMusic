{-# LANGUAGE EmptyDataDecls, 
             MultiParamTypeClasses, 
             GADTs,
             RankNTypes,
             FlexibleInstances #-}

module CsoundExp (noteToTrack, phraseToTrack, musicToTrack, playCsound, writeCsound) where

import Music (mapPhraseSingle, apPitch, apDur, apTran, apInt,
              explodeVoices, splitVoices, mapMusic, Metronome(..),
              AbstractNote(..), Music(..), mapPhrase, noteToSound, Tuning(..),
              revVoices, Note3, AbstractDur3(..), AbstractPitch3(..), AbstractPhrase(..))

import qualified Csound.Base as C
-- import Csound.Catalog
import Csound.Patch


noteToTrack :: Note3 -> C.Track C.D (C.D, C.D)
noteToTrack (AbstractPitch p d) = C.str (durToSeconds d) $ C.temp (1, getFreq p)
noteToTrack (Rest d) = C.rest (durToSeconds d) 

durToSeconds (AbstractDur3 l) = (C.double l)/1000
getFreq (AbstractPitch3 f) = C.double f

phraseToTrack :: AbstractPhrase Note3 -> C.Track C.D (C.D, C.D)
phraseToTrack (AbstractPhrase notes) = C.mel $ phraseToTrack' notes
  where phraseToTrack' [] = []
        phraseToTrack' (n@(AbstractPitch _ _):ns) = (noteToTrack n) : (phraseToTrack' ns)
        phraseToTrack' (n@(Rest _):ns) = (noteToTrack n) : (phraseToTrack' ns)
        phraseToTrack' (n:ns) = phraseToTrack' ns

musicToTrack :: Music Note3 -> C.Track C.D (C.D, C.D)
musicToTrack (Start phrase) = musicToTrack $ explodeVoices (Start phrase)
musicToTrack (Voices phrases) = C.har $ map phraseToTrack phrases

-- playCsoundsDemo q = C.dac $ C.mix $ C.sco h q
  -- where h x = return $ harpsichord x

playCsoundsInstr i q = C.mix $ C.atSco i q

playCsound i m = C.dac $ playCsoundsInstr i $ musicToTrack m

writeCsound f i m = C.writeSnd f $ playCsoundsInstr i $ musicToTrack m
