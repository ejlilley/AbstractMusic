{-# LANGUAGE EmptyDataDecls, 
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

module Lilypond (lilypond) where

import Music (AbstractNote(..),Note2(..), Name(..), Accidental(..), AbstractPitch2(..), AbstractDur2(..), AbstractPhrase(..), Note(..))
import Data.Ratio
import Data.Char
import qualified Music.Lilypond as L
import Text.Pretty hiding (int)

class Lily t where
  toLily :: t -> L.Music

instance Lily Note2 where
  toLily (AbstractPitch p d) = L.Note (toLPitch p) (Just (toLDur d)) []
  toLily (Rest d) = L.Rest (Just (toLDur d)) []

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
toLOctave (Down n) = (toLOctave n) + 1
toLOctave _ = 4

toLDur (AbstractDur2 r) = L.Duration r

instance Lily (AbstractPhrase Note2) where
  toLily (AbstractPhrase ns) = L.Sequential (map toLily ns)

lilypond n = (pretty . toLily) n
