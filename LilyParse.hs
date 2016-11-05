{-# LANGUAGE FlexibleContexts,
             OverlappingInstances,
             TypeSynonymInstances,
             FlexibleInstances #-}


module LilyParse (LilyFile, LilyTopLevel(..), LilyRelative(..), LilyNoteName(..),
                  LilyOctave(..), LilyNote(..), LilyTie(..), LilyBar(..),
                  LilyKey(..), LilyClef(..), LilySequential(..), LilySimultaneous(..),
                  LilyChord(..), LilyVoice(..), LilyTuplet(..), LilyStaff(..),
                  LilyChange(..), LilyContext(..), LilyExpr(..), LilyExpressive(..),
                  LilyArticulation(..), LilySlur(..), LilyLiteral(..), LilyAssignment(..),
                  LilyIdentifier(..), LilyNoteDuration, LilyFixed(..), LilyGrace(..),
                  LilyLyrics(..), LilyTranspose(..),
                  parseNoteName, parseNoteDurs, parseOctave, parseNote,
                  parseNotes, parseSeq, parseSim, parseChord,
                  parseBar, parseTie, parseContext, parseAssign,
                  parseLiteral, parseExpressive, parseArticulation, parseSlur,
                  parseScore, parseVersion, parseVoice, parseStaff,
                  parseChange, parseClef, parseKey, parseRelative,
                  parseExpr, parseExprs, parseTopLevel, parseLily,
                  expandVariables, expandRel, expandDur,
                  mapNotes, mapIdents, mapSeqs, mapExprs
                 ) where

import Prelude hiding (negate)

import Data.Ratio
import Data.List
import Data.Maybe
import Data.Char (isSpace)
import Control.Applicative hiding (many, optional, (<|>))
import Control.Monad (MonadPlus(..), ap)

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Text.Parsec.Language (haskell) -- use Haskell-style floats etc. for now

-- import Music (AbstractNote(..),Note2(..), Name(..), Accidental(..),
              -- AbstractPitch2(..), AbstractDur2(..), AbstractInt2(..), AbstractPhrase(..),
              -- Note(..), Music(..), Pitch(..), Interval(..), explodeVoices, apRest)
-- import Shortcuts


-- instance Applicative (GenParser s a) where
  -- pure = return
  -- (<*>) = ap
-- 
-- instance Alternative (GenParser s a) where
  -- empty = mzero
  -- (<|>) = mplus

--------------------------------

type LilyFile = [LilyTopLevel]

type LilyComment = String

instance Show LilyFile where
  show f = showList' "\n" f

-- Top level expressions in a Lilypond file:
data LilyTopLevel = Header [LilyAssignment]
                  | Score LilyExpr
                  | Paper
                  | Midi
                  | Layout
                  | Assignment LilyAssignment -- variable assigment
                  | Scheme LilyLiteral
                  | Version String

instance Show LilyTopLevel where
  show (Assignment a) = show a
  show (Scheme a) = show a
  show (Score a) = "\\score " ++ (show a)
  show (Version s) = "\\version \"" ++ s ++ "\""

data LilyRelative = LilyRelative (Maybe (LilyNoteName, LilyOctave)) LilyExpr

instance Show LilyRelative where
  show (LilyRelative Nothing a) = "\\relative " ++ (show a)
  show (LilyRelative (Just (n,o)) a) = "\\relative " ++ (show n) ++ (show o) ++ " " ++ (show a)

data LilyFixed = LilyFixed (Maybe (LilyNoteName, LilyOctave)) LilyExpr

instance Show LilyFixed where
  show (LilyFixed Nothing a) = "\\fixed " ++ (show a)
  show (LilyFixed (Just (n,o)) a) = "\\fixed " ++ (show n) ++ (show o) ++ " " ++ (show a)

data LilyTranspose = LilyTranspose (LilyNoteName, LilyOctave) (LilyNoteName, LilyOctave) LilyExpr

instance Show LilyTranspose where
  show (LilyTranspose (n,o) (n',o') e) = "\\transpose " ++ (show n) ++ (show o) ++ " " ++ (show n') ++ (show o') ++ " " ++ (show e)

data LilyNoteName = L_A | L_B | L_C | L_D | L_E | L_F | L_G
                  | L_AIS | L_BIS | L_CIS | L_DIS | L_EIS | L_FIS | L_GIS
                  | L_AES | L_BES | L_CES | L_DES | L_EES | L_FES | L_GES
                  | L_AISIS | L_BISIS | L_CISIS | L_DISIS | L_EISIS | L_FISIS | L_GISIS
                  | L_AESES | L_BESES | L_CESES | L_DESES | L_EESES | L_FESES | L_GESES
                  | NOPE -- not a note
                  deriving Eq

instance Show LilyNoteName where
  show n = fromLilyNoteName n

data LilyOctave = LilyOctave Int
                | LilyOctaveCheck Int
                deriving Eq

instance Show LilyOctave where
  show (LilyOctave o) = printOctave o
  show (LilyOctaveCheck o) = "=" ++ (printOctave o)

type LilyNoteDuration = Ratio Int
  -- in multiples of 1 semibreve, where -1 means "no duration
  -- indicated, use previous note's duration"
-- instance Show LilyNoteDuration where
  -- show (

data LilyNote = LilyNote LilyNoteName LilyOctave LilyNoteDuration [LilyExpressive]
              | LilyRest LilyNoteDuration
              | LilyFullRest LilyNoteDuration
              | LilySkip LilyNoteDuration

data LilyTie = LilyTie

instance Show LilyTie where
  show LilyTie = "~"

data LilyBar = LilyBar (Maybe String)

instance Show LilyBar where
  show (LilyBar Nothing) = "|\n"
  show (LilyBar (Just s)) = "\\bar \"" ++ s ++ "\"\n"

data LilyKey = Major LilyNoteName
             | Minor LilyNoteName

instance Show LilyKey where
  show (Major n) = "\\key " ++ (show n) ++ " \\major"
  show (Minor n) = "\\key " ++ (show n) ++ " \\minor"

data LilyClef = Treble | Bass | Soprano | Alto | Tenor | ArbitraryClef String

instance Show LilyClef where
  show Treble = "\\clef treble"
  show Bass = "\\clef bass"
  show Soprano = "\\clef soprano"
  show Alto = "\\clef alto"
  show Tenor = "\\clef tenor"
  show (ArbitraryClef s) = "\\clef \"" ++ s ++ "\""

type LilyTime = (Int, LilyNoteDuration) -- i.e. "3/4" = "3 crotchets" etc.

instance Show LilyTime where
  show (x, d) = "\\time " ++ (show x) ++ "/" ++ (show d) -- TODO: fix this

printOctave x
  | x > 0 = replicate (fromIntegral x) '\''
  | x < 0 = replicate ((*(-1)) (fromIntegral x)) ','
  | True = ""

showDur :: Ratio Int -> String
showDur d | (d < 0) = ""
          | (d == 4) = "\\longa"
          | (d == 6) = "\\longa."
          | (d == 7) = "\\longa.."
          | (d == 2) = "\\breve"
          | (d == 3) = "\\breve."
          | (d == 7 % 2) = "\\breve.."
          | ((numerator d) == 1) = show (denominator d)
          | ((numerator d) == 3) = let x = denominator d
                                       y = (numerator d) - 1
                                   in (showDur $ y % x) ++ "."
          | ((numerator d) == 7) = let x = denominator d
                                       y = (numerator d) - 3
                                   in (showDur $ y % x) ++ ".."
          | otherwise = show (d)

instance Show LilyNote where
  show (LilyNote n o d e) = (fromLilyNoteName n)
                          ++ (show o)
                          ++ (showDur d)
                            ++ (showList' "" e)
  show (LilyRest d) = "r" ++ (showDur d)
  show (LilyFullRest d) = "R" ++ (showDur d)
  show (LilySkip d) = "s" ++ (showDur d)

-- expressions enclosed by curly brackets, separated by spaces { a'2 e }
data LilySequential = LilySequential [LilyExpr]

showList' _ [] = ""
showList' s l = foldl1 (\x y -> x ++ s ++ y) $ map show l

instance Show LilySequential where
  show (LilySequential l) = "{ " ++ (showList' " " l) ++ " }"

-- expressions enclosed by double angle brackets, separated by double
-- slashes: << {a'2 e} \\ {b c} \\ {d e} >>
data LilySimultaneous = LilySimultaneous [LilyExpr]

instance Show LilySimultaneous where
  show (LilySimultaneous l) = "<< " ++ (showList' " \\\\ " l) ++ " >>"

data LilyChord = LilyChord [LilyNote] LilyNoteDuration -- chords <a'2 e>4

instance Show LilyChord where
  show (LilyChord l d) = "<" ++ (showList' " " l) ++ ">" ++ (showDur d)

data LilyVoice = LilyVoice (Maybe String) (Maybe LilyContext) LilyExpr

instance Show LilyVoice where
  show (LilyVoice (Just s) (Just c) e) = "\\new Voice = \"" ++ s ++ "\" " ++ (show c) ++ " " ++ (show e)
  show (LilyVoice Nothing (Just c) e) = "\\new Voice " ++ (show c) ++ " " ++ (show e)
  show (LilyVoice (Just s) Nothing e) = "\\new Voice = \"" ++ s ++ "\" " ++ (show e)
  show (LilyVoice Nothing Nothing e) = "\\new Voice = " ++ (show e)

-- other types of brackets are begin/end of slurs etc., so not for nesting expressions

data LilyTuplet = LilyTuplet (Ratio Int) LilyExpr

instance Show LilyTuplet where
  show (LilyTuplet r e) = "\\tuplet " ++ (show r) ++ " " ++ (show e)

data LilyStaff = LilyStaff (Maybe String) (Maybe LilyContext) LilyExpr -- \new Staff = "staff name" {a '2 e}
               -- | LilyPianoStaff (Maybe String) (Maybe LilyContext) LilyExpr

instance Show LilyStaff where
  show (LilyStaff (Just s) (Just c) e) = "\\new Staff = \"" ++ s ++ "\" " ++ (show c) ++ " " ++ (show e)
  show (LilyStaff Nothing (Just c) e) = "\\new Staff " ++ (show c) ++ " " ++ (show e)
  show (LilyStaff (Just s) Nothing e) = "\\new Staff = \"" ++ s ++ "\" " ++ (show e)
  show (LilyStaff Nothing Nothing e) = "\\new Staff = " ++ (show e)

data LilyLyrics = LilyLyrics String LilyExpr -- \new Lyrics \lyricsto "foo" { bar }

instance Show LilyLyrics where
  show (LilyLyrics s e) = "\\new Lyrics \\lyricsto \"" ++ s ++ "\" " ++ (show e)

data LilyChange = LilyChange String -- \change Staff = "foo"

instance Show LilyChange where
  show (LilyChange s) = "\\change Staff = \"" ++ s ++ "\""

data LilyContext = LilyContext [LilyAssignment] -- \with { alignAboveContext = #"main" } or whatever

instance Show LilyContext where
  show (LilyContext as) = "\\with { " ++ (showList' "\n" $ map show as) ++ " }"

data LilyExpr = Note LilyNote
              | Seq LilySequential
              | Sim LilySimultaneous
              | Chord LilyChord
              | Tie LilyTie
              | Bar LilyBar
              | Time LilyTime
              | Key LilyKey
              | Clef LilyClef
              | Ident LilyIdentifier
              | Tup LilyTuplet
              | Staff LilyStaff
              | Voice LilyVoice
              | Lyrics LilyLyrics
              | Change LilyChange
              | Rel LilyRelative
              | Trans LilyTranspose
              | Fix LilyFixed
              | Lit LilyLiteral
              | Assign LilyAssignment
              | Grace LilyGrace
              | LyricMode String
              | SlurExpr LilySlur
              | LayoutExpr
              | MidiExpr

instance Show LilyExpr where
  show (Note n) = show n
  show (Seq s) = show s
  show (Sim s) = show s
  show (Chord s) = show s
  show (Tie s) = show s
  show (Bar s) = show s
  show (Ident s) = show s
  show (Lit s) = show s
  show (Voice s) = show s
  show (Staff s) = show s
  show (Time s) = show s
  show (Key s) = show s
  show (Clef s) = show s
  show (Change s) = show s
  show (Assign s) = show s
  show (Rel s) = show s
  show (Trans s) = show s
  show (Grace s) = show s
  show (Tup s) = show s
  show (Lyrics s) = show s
  show (LyricMode s) = "\\lyricmode {" ++ (show s) ++ "}"
  show (SlurExpr s) = show s
  show LayoutExpr = "\\layout { }"
  show MidiExpr = "\\midi { }"

-- something 'expressive' attached to a note, not a music expression!
data LilyExpressive = Super LilyExpressive -- ^something
                    | Sub LilyExpressive -- _something
                    | Markup String -- \markup{foo}
                    -- | Dynamics String -- \mf
                    | Artic LilyArticulation -- -+ -- -. etc.
                    | Expr LilyIdentifier -- \anything, i.e. ornaments, dynamics etc.
                    | Slur LilySlur -- \( \[ etc.

instance Show LilyExpressive where
  show (Super e) = "^" ++ (show e)
  show (Sub e) = "_" ++ (show e)
  show (Markup e) = "\\markup{" ++ (show e) ++ "}"
  show (Artic e) = "-" ++ (show e)
  show (Expr e) = show e
  show (Slur e) = show e

-- these can be redefined to refer to any expressive mark
data LilyArticulation = ArticHat
                      | ArticPlus
                      | ArticDash
                      | ArticBar
                      | ArticLarger
                      | ArticDot
                      | ArticUnderscore

instance Show LilyArticulation where
  show ArticHat = "^"
  show ArticPlus = "+"
  show ArticDash = "-"
  show ArticLarger = ">"
  show ArticDot = "."
  show ArticUnderscore = "_"
  show ArticBar = "|"

data LilyGrace = BeforeGrace LilyExpr LilyNote
               | AfterGrace LilyNote LilyExpr

instance Show LilyGrace where
  show (BeforeGrace e n) = "\\grace " ++ (show e) ++ " " ++ (show n)
  show (AfterGrace n e) = "\\afterGrace " ++ (show n) ++ " " ++ (show e)

data LilySlur = SlurOn | SlurOff | BeamOn | BeamOff -- etc.

instance Show LilySlur where
  show SlurOn = "\\("
  show SlurOff = "\\)"
  show BeamOn = "\\["
  show BeamOff = "\\]"

data LilyLiteral = LilyFloat Double -- #0.4
                 | LilyInt Int -- #t
                 | LilyBool Bool -- #t
                 | LilyString String -- etc.
                 | LilySymbol String -- etc.
                 | LilySexp String -- etc.

instance Show LilyLiteral where
  show (LilyFloat d) = "#" ++ (show d)
  show (LilyInt d) = "#" ++ (show d)
  show (LilyBool True) = "##t"
  show (LilyBool False) = "##f"
  show (LilyString d) = "#\"" ++ d ++ "\""
  show (LilySymbol d) = "#\'" ++ d
  show (LilySexp d) = "#(" ++ d ++ ")"

data LilyAssignment = LilyAssignment String LilyExpr -- \set foo = {a b c}
                    | LilySymbAssignment String LilyLiteral LilyExpr -- foo #'bar = baz
                    | LilyOverride LilyAssignment -- \override ...
                    | LilyOnce LilyAssignment -- \once ...

type LilyDict = [LilyAssignment]

instance Show LilyAssignment where
  show (LilyAssignment s e) = s ++ " = " ++ (show e)
  show (LilyOverride a) = "\\override " ++ (show a)
  show (LilyOnce a) = "\\once " ++ (show a)
  show (LilySymbAssignment s l e) = s ++ " " ++ (show l) ++ " = " ++ (show e)

data LilyIdentifier = LilyIdentifier String -- \foo

instance Show LilyIdentifier where
  show (LilyIdentifier s) = "\\" ++ s

--------------------------------

toLilyNoteName :: String -> LilyNoteName
toLilyNoteName "a"     = L_A    
toLilyNoteName "b"     = L_B    
toLilyNoteName "c"     = L_C    
toLilyNoteName "d"     = L_D    
toLilyNoteName "e"     = L_E    
toLilyNoteName "f"     = L_F    
toLilyNoteName "g"     = L_G    
toLilyNoteName "ais"   = L_AIS  
toLilyNoteName "bis"   = L_BIS  
toLilyNoteName "cis"   = L_CIS  
toLilyNoteName "dis"   = L_DIS  
toLilyNoteName "eis"   = L_EIS  
toLilyNoteName "fis"   = L_FIS  
toLilyNoteName "gis"   = L_GIS  
toLilyNoteName "aes"   = L_AES  
toLilyNoteName "bes"   = L_BES  
toLilyNoteName "ces"   = L_CES  
toLilyNoteName "des"   = L_DES  
toLilyNoteName "ees"   = L_EES  
toLilyNoteName "fes"   = L_FES  
toLilyNoteName "ges"   = L_GES  
toLilyNoteName "aisis" = L_AISIS
toLilyNoteName "bisis" = L_BISIS
toLilyNoteName "cisis" = L_CISIS
toLilyNoteName "disis" = L_DISIS
toLilyNoteName "eisis" = L_EISIS
toLilyNoteName "fisis" = L_FISIS
toLilyNoteName "gisis" = L_GISIS
toLilyNoteName "aeses" = L_AESES
toLilyNoteName "beses" = L_BESES
toLilyNoteName "ceses" = L_CESES
toLilyNoteName "deses" = L_DESES
toLilyNoteName "eeses" = L_EESES
toLilyNoteName "feses" = L_FESES
toLilyNoteName "geses" = L_GESES
toLilyNoteName _ = NOPE

fromLilyNoteName :: LilyNoteName -> String
fromLilyNoteName L_A     = "a"    
fromLilyNoteName L_B     = "b"    
fromLilyNoteName L_C     = "c"    
fromLilyNoteName L_D     = "d"    
fromLilyNoteName L_E     = "e"    
fromLilyNoteName L_F     = "f"    
fromLilyNoteName L_G     = "g"    
fromLilyNoteName L_AIS   = "ais"  
fromLilyNoteName L_BIS   = "bis"  
fromLilyNoteName L_CIS   = "cis"  
fromLilyNoteName L_DIS   = "dis"  
fromLilyNoteName L_EIS   = "eis"  
fromLilyNoteName L_FIS   = "fis"  
fromLilyNoteName L_GIS   = "gis"  
fromLilyNoteName L_AES   = "aes"  
fromLilyNoteName L_BES   = "bes"  
fromLilyNoteName L_CES   = "ces"  
fromLilyNoteName L_DES   = "des"  
fromLilyNoteName L_EES   = "ees"  
fromLilyNoteName L_FES   = "fes"  
fromLilyNoteName L_GES   = "ges"  
fromLilyNoteName L_AISIS = "aisis"
fromLilyNoteName L_BISIS = "bisis"
fromLilyNoteName L_CISIS = "cisis"
fromLilyNoteName L_DISIS = "disis"
fromLilyNoteName L_EISIS = "eisis"
fromLilyNoteName L_FISIS = "fisis"
fromLilyNoteName L_GISIS = "gisis"
fromLilyNoteName L_AESES = "aeses"
fromLilyNoteName L_BESES = "beses"
fromLilyNoteName L_CESES = "ceses"
fromLilyNoteName L_DESES = "deses"
fromLilyNoteName L_EESES = "eeses"
fromLilyNoteName L_FESES = "feses"
fromLilyNoteName L_GESES = "geses"
fromLilyNoteName NOPE = "????"

noteNames = ["aisis", "bisis", "cisis", "disis", "eisis", "fisis", "gisis",
             "aeses", "beses", "ceses", "deses", "eeses", "feses", "geses",
             "ais"  , "bis"  , "cis"  , "dis"  , "eis"  , "fis"  , "gis",
             "aes"  , "bes"  , "ces"  , "des"  , "ees"  , "fes"  , "ges",
             "a"    , "b"    , "c"    , "d"    , "e"    , "f"    , "g"]

parseNoteName = foldl1 (<|>) $ map try $ map string noteNames
                

parseBaseDur = foldl1 (<|>) $ map try $ map string $ map show $ reverse $ map (2^) [0..6]

longDurs = [("breve..", 7 % 2),
            ("breve.", 3),
            ("breve", 2),
            ("longa..", 7),
            ("longa.", 6),
            ("longa", 4)]

parseLongDur = (char '\\') *> (foldl1 (<|>) $ map try $ map string $ map fst longDurs)

readLongDur d = case lookup d longDurs of
                 (Just x) -> x
                 Nothing -> error "Unknown long note duration name"

dotted x = 3*(readDur x)/2
doubleDotted x = 7*(readDur x)/4

readDur :: String -> Ratio Int
readDur x = 1/(fromIntegral (read x))
-- parseNoteDurs = readInt <$> (foldl1 (<|>) $ map try $ map string $
                             -- (map (dot . dot) validDurs) ++ (map dot validDurs) ++ validDurs)

parseNoteDurs = try (doubleDotted <$> parseBaseDur <* (char '.') <* (char '.'))
                <|> try (dotted <$> parseBaseDur <* (char '.'))
                <|> try (readLongDur <$> parseLongDur)
                <|> (readDur <$> parseBaseDur)

parseOctave :: Parsec String () LilyOctave
parseOctave = try (LilyOctaveCheck <$> (char '=' *> parseOctave')) <|> (LilyOctave <$> parseOctave')
  where parseUpOctave = (fromIntegral . length) <$> many1 (char '\'')
        parseDownOctave = (fromIntegral . (*(-1)) . length) <$> many1 (char ',')
        parseOctave' = parseUpOctave <|> parseDownOctave <|> return 0


singlet x = [x]
collapse = foldl (++) []

parseNote :: Parsec String () LilyNote
parseNote = (LilyRest <$> (char 'r' *> (parseNoteDurs <|> return (-1))))
            <|> (LilyFullRest <$> (char 'R' *> (parseNoteDurs <|> return (-1))))
            <|> (LilySkip <$> (char 's' *> (parseNoteDurs <|> return (-1))))
            <|> (LilySkip <$> (string "\\skip" *> sorc *> (parseNoteDurs <|> return (-1))))
            <|> (LilyNote <$> (toLilyNoteName <$> parseNoteName)
                 <*> ((optional (char '!')) *> parseOctave <* (optional (char '!')))
                 <*> (parseNoteDurs <|> return (-1))
                 <*> (many parseExpressive <|> return []))

parseNoteMult = (flip replicate) <$> parseNote <*> (char '*' *> parseLitInt)

multNoteDur (LilyNote n o d e) r = LilyNote n o (d*r) e
multNoteDur (LilyRest d) r = LilyRest (d*r)
multNoteDur (LilyFullRest d) r = LilyFullRest (d*r)
multNoteDur (LilySkip d) r = LilySkip (d*r)

parseNoteFrac = multNoteDur <$> parseNote <*> ((%) <$> (char '*' *> parseLitInt) <*> (char '/' *> parseLitInt))

parseNotes = collapse <$> (many (sorc *> (    (try (singlet <$> parseNoteFrac))
                                          <|> (try parseNoteMult)
                                          <|> (singlet <$> parseNote)) <* sorc))

-- parseNotes = many (spaces *> parseNote <* spaces)

parseSeq :: Parsec String () LilySequential
parseSeq = LilySequential <$> ((char '{') *> parseExprs <* (char '}'))

parseSim :: Parsec String () LilySimultaneous
parseSim = LilySimultaneous <$> ((string "<<") *> (try parseExprsSlashes <|> parseExprs) <* (string ">>"))

parseChord :: Parsec String () LilyChord
parseChord = LilyChord <$> ((char '<') *> parseNotes <* (char '>')) <*> (parseNoteDurs <|> return (-1))

parseBar = LilyBar <$> ((Nothing <$ (char '|'))
                        <|> (Just <$> (string "\\bar" *> spaces *> parseLitString)))

parseTie = LilyTie <$ (char '~' <* sorc)

-- parseTie = LilyTie <$ (char '~' <* sorc <* lookAhead
                       -- ((Note <$> parseNote) <|> (Chord <$> parseChord)))
         -- should really required that a note/chord comes *before*
         -- the tie too, not sure how to do this (TODO)

braces'  = between ((char '{') <* sorc) (sorc *> (char '}'))
parseContext = LilyContext <$> ((string "\\with") *> sorc *> (braces' (many parseAssign)))

-- parseAssign = (LilyOverride <$> (string "\\override" *> spaces *> (many $ oneOf validIdentifiers)) <*> (spaces *> (char '=') *> spaces *> parseExpr))
              -- <|> (LilyAssignment <$> (many $ oneOf validIdentifiers) <*> (spaces *> (char '=') *> spaces *> parseExpr))

reservedVariables = ["incipitwidth", "htitle", "hcomposer", "title",
                     "subtitle", "subsubtitle", "composer", "opus",
                     "poet", "copyright"]

parseReservedVariable = foldl1 (<|>) $ map try $ map string reservedVariables

parseAssign =     try (LilyOverride <$> (string "\\override" *> sorc *> parseAssign))
              <|> try (LilyOnce <$> (string "\\once" *> sorc *> parseAssign))
              <|> try (LilySymbAssignment <$> (many $ oneOf validIdentifiers) <*> (sorc *> parseLiteral) <*> (sorc *> (char '=') *> sorc *> parseExpr))
              <|> try (LilyAssignment <$> parseReservedVariable <*> ((Lit . LilyInt) <$> (sorc *> (char '=') *> sorc *> parseLitInt)))
              <|> try (LilyAssignment <$> parseReservedVariable <*> ((Lit . LilyString) <$> (sorc *> (char '=') *> sorc *> parseLitString)))
              -- <|> try (LilyAssignment <$> parseReservedVariable <*> ((Lit . LilyString) <$> (sorc *> (char '=') *> sorc *> parseMarkup)))
              <|> try (LilyAssignment <$> ((string "\\set") *> sorc *> (many $ oneOf validIdentifiers)) <*> (Lit <$> (sorc *> (char '=') *> sorc *> parseLiteral)))
              <|> (LilyAssignment <$> (many $ oneOf validIdentifiers) <*> (sorc *> (char '=') *> sorc *> parseExpr))

parens'  = between ((char '(') <* spaces) (spaces *> (char ')'))
parseLitBool = try (True <$ (string "#t")) <|> (False <$ (string "#f"))
parseLitFloat = try ((*(-1)) <$> ((char '-') *> (float haskell)))
                <|> (float haskell)
parseLitInt = fromIntegral <$> (integer haskell)
parseLitString = stringLiteral haskell
parseLitSymbol = (char '\'') *> (many $ oneOf validIdentifiers)

sexpChars = validIdentifiers ++ ['0'..'9'] ++ [' ', '-', '\'', '\"', ':', '#'] -- ???? TODO: fix
parseLitSexp = parens' (many $ oneOf sexpChars)

-- parseLiteral = (char '#') *> (    try (LilyBool <$> parseLitBool)
                              -- <|> try (LilyFloat <$> parseLitFloat)
                              -- <|> try (LilyInt <$> parseLitInt)
                              -- <|> try (LilyString <$> parseLitString)
                              -- <|> try (LilySymbol <$> parseLitSymbol)
                              -- <|> (LilySexp <$> parseLitSexp))

parseLiteral = try (LilyBool <$> parseLitBool)
               <|> try (LilyFloat <$> parseLitFloat)
               <|> try (LilyInt <$> parseLitInt)
               <|> try (LilyString <$> parseLitString)
               <|> try (LilySymbol <$> parseLitSymbol)
               <|> try (LilySexp <$> parseLitSexp)
               <|> ((char '#') *> parseLiteral)


-- TODO: this doesn't parse "--" at the end of a string of expressions, fix
parseExpressive =     try (Super <$> parseSuperText)
                  <|> try (Sub <$> parseSubText)
                  <|> try (Markup <$> parseMarkup)
                  <|> try (Artic <$> parseArticulation)
                  <|> try (Slur <$> parseSlur)
                  <|> (Expr <$> parseIdent)

parseSuperText = Markup <$> (char '^' *> parseLitString)

parseSubText = Markup <$> (char '_' *> parseLitString)

parseMarkup = string "\\markup" *> (braces' parseLitString)

parseGrace = try ((string "\\afterGrace") *> (AfterGrace <$> (sorc *> parseNote) <*> (sorc *> parseExpr)))
             <|> ((string "\\grace") *> (BeforeGrace <$> (sorc *> parseExpr) <*> (sorc *> parseNote)))

parseFrac = (%) <$> parseLitInt <*> ((char '/') *> parseLitInt) 

parseTuplet = (try (string "\\tuplet") <|> (string "\\times")) *> (LilyTuplet <$> (sorc *> parseFrac) <*> (sorc *> parseExpr))

parseArticulation = (char '-') *>     ((ArticHat <$ char '^')
                                   <|> (ArticPlus <$ char '+')
                                   <|> (ArticDash <$ char '-')
                                   <|> (ArticBar <$ char '|')
                                   <|> (ArticLarger <$ char '>')
                                   <|> (ArticDot <$ char '.')
                                   <|> (ArticUnderscore <$ char '_'))

-- parseSlur = (char '\\') *> ((SlurOn <$ char '(')
                            -- <|> (SlurOff <$ char ')')
                            -- <|> (BeamOn <$ char '[')
                            -- <|> (BeamOff <$ char ']'))

parseSlur = (    try (SlurOn <$ string "\\(")
             <|> try (SlurOff <$ string "\\)")
             <|> try (BeamOn <$ string "\\[")
             <|> try (BeamOff <$ string "\\]")
             <|> try (SlurOn <$ char '(')
             <|> try (SlurOff <$ char ')')
             <|> try (BeamOn <$ char '[')
             <|> (BeamOff <$ char ']')
            )

parseScore = (string "\\score" *> sorc) *> parseExpr

parseLayout = (string "\\layout" *> sorc) *> parseExpr

parseMidi = (string "\\midi" *> sorc) *> parseExpr

parseVersion = (string "\\version" *> sorc) *> parseLitString

-- \new Voice = "foo" \with { contexts } { notes }
parseVoice' = (string "\\new") *> sorc *> (string "Voice") *> sorc
parseVoice = LilyVoice <$> (parseVoice' *> ((try ((char '=') *> sorc *> (Just <$> parseLitString))) <|> return Nothing))
             <*> (sorc *> (try (Just <$> parseContext)) <|> return Nothing)
             <*> (sorc *> parseExpr)

staffNames = ["Staff", "PianoStaff", "ChoirStaff"]
parseStaffNames = foldl1 (<|>) $ map try $ map string staffNames

parseStaff' = (string "\\new") *> sorc *> parseStaffNames *> sorc
parseStaff = LilyStaff <$> (parseStaff' *> ((try ((char '=') *> sorc *> (Just <$> parseLitString))) <|> return Nothing))
             <*> (sorc *> (try (Just <$> parseContext)) <|> return Nothing)
             <*> (sorc *> parseExpr)

parseChange' = (string "\\change") *> sorc *> (string "Staff") *> sorc
parseChange = LilyChange <$> (parseChange' *> (char '=') *> sorc *> parseLitString)

validIdentifiers = ['a'..'z'] ++ ['A'..'Z'] ++ ['.', '-']

parseIdent = LilyIdentifier <$> ((char '\\') *> (many1 $ oneOf validIdentifiers))

parseTime :: Parsec String () LilyTime
parseTime = (,) <$> (string "\\time" *> sorc *> parseLitInt)
            <*> (sorc *> char '/' *> (readDur <$> parseBaseDur))

parseClef :: Parsec String () LilyClef
parseClef = (string "\\clef" *> sorc) *> (
      try (Treble <$ string "treble")
  <|> try (Bass <$ string "bass")
  <|> try (Soprano <$ string "soprano")
  <|> try (Alto <$ string "alto")
  <|> try (Tenor <$ string "tenor")
  <|> (ArbitraryClef <$> parseLitString)
      )

parseKey :: Parsec String () LilyKey
parseKey = try (Major <$> ((string "\\key" *> sorc) *> (toLilyNoteName <$> parseNoteName) <* sorc <* (string "\\major")))
           <|> Minor <$> ((string "\\key" *> sorc) *> (toLilyNoteName <$> parseNoteName) <* sorc <* (string "\\minor"))

parseRelative = LilyRelative <$> (try (Just <$> ((,) <$> ((string "\\relative" *> sorc) *> (toLilyNoteName <$> parseNoteName)) <*> parseOctave))
                                  <|> (string "\\relative" *> sorc *> return Nothing)) <*> (sorc *> parseExpr)

parseTranspose = LilyTranspose
                 <$> ((,) <$> ((string "\\transpose" *> sorc) *> (toLilyNoteName <$> parseNoteName)) <*> parseOctave)
                 <*> ((,) <$> (toLilyNoteName <$> (sorc *> parseNoteName)) <*> parseOctave)
                 <*> (sorc *> parseExpr)

lyricChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '-', '_', ',', '.', '\\', '!', '?', '\n']

parseLyricmode = string "\\lyricmode" *> sorc *> braces' (many $ oneOf lyricChars)

parseLyrics = LilyLyrics <$> ((string "\\new") *> sorc *> (string "Lyrics") *> sorc *> (string "\\lyricsto") *> sorc *> parseLitString) <*> (sorc *> parseExpr)

-- just one expression
parseExpr :: Parsec String () LilyExpr
parseExpr =     try (Seq <$> parseSeq)
            <|> try (Sim <$> parseSim)
            <|> try (Chord <$> parseChord)
            <|> try (Note <$> parseNote)
            <|> try (Tie <$> parseTie)
            <|> try (Bar <$> parseBar)
            <|> try (Time <$> parseTime)
            <|> try (Clef <$> parseClef)
            <|> try (Key <$> parseKey)
            <|> try (Voice <$> parseVoice)
            <|> try (Staff <$> parseStaff)
            <|> try (Change <$> parseChange)
            <|> try (Rel <$> parseRelative)
            <|> try (Trans <$> parseTranspose)
            <|> try (Grace <$> parseGrace)
            <|> try (Lyrics <$> parseLyrics)
            <|> try (LyricMode <$> parseLyricmode)
            <|> try (Tup <$> parseTuplet)
            <|> try (Assign <$> parseAssign)
            <|> try (Lit <$> parseLiteral)
            <|> try (SlurExpr <$> parseSlur)
            <|> try (LayoutExpr <$ parseLayout)
            <|> try (MidiExpr <$ parseMidi)
            <|> (Ident <$> parseIdent) -- put this last, after all reserved backslash-commands
            <?> "music expression"

-- a list of expressions, delimited by any amount of whitespace

-- parseExprs = many $ spaces *> parseExpr <* spaces

parseExprs = try (collapse <$> (many $ sorc *> (    try ((singlet . Note) <$> parseNoteFrac)
                                                <|> try ((map Note) <$> parseNoteMult)
                                                <|> (singlet <$> parseExpr)) <* sorc))
             <|> ([] <$ sorc)

-- parseExprs' = many $ sorc *> ((try $ (map Note) <$> parseNoteMult) <|> (singlet <$> parseExpr)) <* sorc

-- a list of at least TWO expressions, delimited by whitespace & double backslashes
-- parseExprsSlashes = (sorc *> parseExpr <* sorc) `sepBy1` (string "\\\\")
parseExprsSlashes = (:) <$> (sorc *> parseExpr <* sorc <* (string "\\\\") <* sorc) <*> ((sorc *> parseExpr <* sorc) `sepBy1` (string "\\\\"))

-- a single top-level expression
parseTopLevel :: Parsec String () LilyTopLevel
parseTopLevel =     try (Score <$> parseScore)
                <|> try (Header <$> parseHeader)
                <|> try (Version <$> parseVersion)
                <|> try (Scheme <$> parseLiteral)
                <|> try (Layout <$ parseLayout)
                <|> try (Midi <$ parseMidi)
                <|> (Assignment <$> parseAssign) -- put this last, after all reserved words
        <?> "top-level expression"

-- a file is a list of top-level expressions
parseLily :: Parsec String () LilyFile
parseLily = many $ sorc *> parseTopLevel <* sorc

spacesOrComments = skipMany $ comment <|> space'
                   where space' = (satisfy isSpace) >> return ()
                         comment = string "%" >> manyTill anyChar newline >> return ()
sorc = spacesOrComments -- shorthand

parseHeader :: Parsec String () [LilyAssignment]
parseHeader = (string "\\header") *> sorc *> braces' (many parseAssign)

instance Enum LilyNote where
  fromEnum (LilyNote L_G (LilyOctave 0) _ _) = 4
  fromEnum (LilyNote L_F (LilyOctave 0) _ _) = 3
  fromEnum (LilyNote L_E (LilyOctave 0) _ _) = 2
  fromEnum (LilyNote L_D (LilyOctave 0) _ _) = 1
  fromEnum (LilyNote L_C (LilyOctave 0) _ _) = 0
  fromEnum (LilyNote L_B (LilyOctave 0) _ _) = 6
  fromEnum (LilyNote L_A (LilyOctave 0) _ _) = 5

  fromEnum (LilyNote L_G (LilyOctaveCheck 0) _ _) = 4
  fromEnum (LilyNote L_F (LilyOctaveCheck 0) _ _) = 3
  fromEnum (LilyNote L_E (LilyOctaveCheck 0) _ _) = 2
  fromEnum (LilyNote L_D (LilyOctaveCheck 0) _ _) = 1
  fromEnum (LilyNote L_C (LilyOctaveCheck 0) _ _) = 0
  fromEnum (LilyNote L_B (LilyOctaveCheck 0) _ _) = 5
  fromEnum (LilyNote L_A (LilyOctaveCheck 0) _ _) = 6

  fromEnum (LilyNote L_GIS o d e) = fromEnum (LilyNote L_G o d e)
  fromEnum (LilyNote L_FIS o d e) = fromEnum (LilyNote L_F o d e)
  fromEnum (LilyNote L_EIS o d e) = fromEnum (LilyNote L_E o d e)
  fromEnum (LilyNote L_DIS o d e) = fromEnum (LilyNote L_D o d e)
  fromEnum (LilyNote L_CIS o d e) = fromEnum (LilyNote L_C o d e)
  fromEnum (LilyNote L_BIS o d e) = fromEnum (LilyNote L_B o d e)
  fromEnum (LilyNote L_AIS o d e) = fromEnum (LilyNote L_A o d e)

  fromEnum (LilyNote L_GES o d e) = fromEnum (LilyNote L_G o d e)
  fromEnum (LilyNote L_FES o d e) = fromEnum (LilyNote L_F o d e)
  fromEnum (LilyNote L_EES o d e) = fromEnum (LilyNote L_E o d e)
  fromEnum (LilyNote L_DES o d e) = fromEnum (LilyNote L_D o d e)
  fromEnum (LilyNote L_CES o d e) = fromEnum (LilyNote L_C o d e)
  fromEnum (LilyNote L_BES o d e) = fromEnum (LilyNote L_B o d e)
  fromEnum (LilyNote L_AES o d e) = fromEnum (LilyNote L_A o d e)

  fromEnum (LilyNote L_GISIS o d e) = fromEnum (LilyNote L_G o d e)
  fromEnum (LilyNote L_FISIS o d e) = fromEnum (LilyNote L_F o d e)
  fromEnum (LilyNote L_EISIS o d e) = fromEnum (LilyNote L_E o d e)
  fromEnum (LilyNote L_DISIS o d e) = fromEnum (LilyNote L_D o d e)
  fromEnum (LilyNote L_CISIS o d e) = fromEnum (LilyNote L_C o d e)
  fromEnum (LilyNote L_BISIS o d e) = fromEnum (LilyNote L_B o d e)
  fromEnum (LilyNote L_AISIS o d e) = fromEnum (LilyNote L_A o d e)

  fromEnum (LilyNote L_GESES o d e) = fromEnum (LilyNote L_G o d e)
  fromEnum (LilyNote L_FESES o d e) = fromEnum (LilyNote L_F o d e)
  fromEnum (LilyNote L_EESES o d e) = fromEnum (LilyNote L_E o d e)
  fromEnum (LilyNote L_DESES o d e) = fromEnum (LilyNote L_D o d e)
  fromEnum (LilyNote L_CESES o d e) = fromEnum (LilyNote L_C o d e)
  fromEnum (LilyNote L_BESES o d e) = fromEnum (LilyNote L_B o d e)
  fromEnum (LilyNote L_AESES o d e) = fromEnum (LilyNote L_A o d e)

  fromEnum (LilyNote n (LilyOctave o) d e) = if o > 0
                                           then 7 + (fromEnum (LilyNote n (LilyOctave (o-1)) d e))
                                           else -7 + (fromEnum (LilyNote n (LilyOctave (o+1)) d e))

  fromEnum (LilyNote n (LilyOctaveCheck o) d e) = if o > 0
                                                  then 7 + (fromEnum (LilyNote n (LilyOctave (o-1)) d e))
                                                  else -7 + (fromEnum (LilyNote n (LilyOctave (o+1)) d e))

  fromEnum _ = 0
  toEnum _ = LilyNote NOPE (LilyOctave 0) 0 []

-- instance Enum LilyNoteName where
  -- fromEnum L_G = 4
  -- fromEnum L_F = 3
  -- fromEnum L_E = 2
  -- fromEnum L_D = 1
  -- fromEnum L_C = 0
  -- fromEnum L_B = -1
  -- fromEnum L_A = -2
-- 
  -- toEnum 4  = L_G
  -- toEnum 3  = L_F
  -- toEnum 2  = L_E
  -- toEnum 1  = L_D
  -- toEnum 0  = L_C
  -- toEnum (-1) = L_B
  -- toEnum (-2) = L_A
  -- toEnum n = toEnum (n `mod` 7)

-- relativeToAbsolute :: (LilyNoteName, LilyOctave) -> LilyNote -> LilyNote
-- relativeToAbsolute p c = if diff < 4
                         -- then c
                         -- else changeOctave c (-(1 + (diff `div` 7)))
  -- where diff = (fromEnum c) - (fromEnum p)

noteDiff n1 n2 = (fromEnum n2) - (fromEnum n1)
                 -- in if d > 1
                    -- then d + 1
                    -- else if d < 1
                         -- then d - 1
                         -- else d

-- 'p' note is previous note, and is assumed to be already absolute
-- 'c' note is current note, and is assumed to be relative
-- Yeah, this is all janky, especially the fact that the Enum instance is the same for both LilyOctave and LilyOctaveCheck
-- Should really turn all LilyOctaves into LilyOctaveChecks as soon as they are absolute-ised
relativeToAbsolute :: LilyNote -> LilyNote -> LilyNote
relativeToAbsolute _ c@(LilyNote cn (LilyOctaveCheck co) cd ce) = c
relativeToAbsolute p c@(LilyNote cn (LilyOctave 0) cd ce) =
  let -- po = case oct of (LilyOctave o) -> o
                       -- (LilyOctaveCheck o) -> o
      -- c' = LilyNote cn (LilyOctave po) cd ce
      diff = noteDiff p c
      compareDiff d | (d > 0) = LilyNote cn (LilyOctave (-((diff + 3) `quot` 7))) cd ce
                    | (d < 0) = LilyNote cn (LilyOctave (-((diff - 3) `quot` 7))) cd ce
                    | otherwise = c
  in compareDiff diff
relativeToAbsolute p c@(LilyNote cn (LilyOctave co) cd ce) =
  let c' = relativeToAbsolute p (LilyNote cn (LilyOctave 0) cd ce)
  in addOctave c' co
relativeToAbsolute _ _ = error "Need a LilyNote to be the base-pitch for relative-to-absolute conversions."

addOctave (LilyNote n (LilyOctave o) d e) o' = LilyNote n (LilyOctave (o + o')) d e
addOctave (LilyNote n (LilyOctaveCheck o) d e) _ = LilyNote n (LilyOctaveCheck o) d e

octaveNum (LilyNote _ (LilyOctave o) _ _) = o
octaveNum (LilyNote _ (LilyOctaveCheck o) _ _) = o

-- relativeToAbsolute :: LilyNote -> LilyNote -> LilyNote
-- relativeToAbsolute p@(LilyNote _ _ _ _) c@(LilyNote _ _ _ _) = addOctave c (octaveNum p)
-- relativeToAbsolute p@(LilyNote _ _ _ _) c = c
-- relativeToAbsolute _ _ = error "Need a LilyNote to be the base-pitch for relative-to-absolute conversions."

-- relativeList base [] = []
-- relativeList base (n@(LilyNote _ _ _ _):ns) = let n' = relativeToAbsolute base n
                                              -- in n' : (relativeList n' ns)
-- relativeList base (n:ns) = relativeList base ns

relativeSeq :: LilyNote -> LilySequential -> (LilyNote, LilySequential)
relativeSeq base (LilySequential []) = (base, LilySequential [])
relativeSeq base (LilySequential (e:es)) = let (b, e') = relativeExpr base e
                                               (b', (LilySequential es')) = relativeSeq b (LilySequential es)
                                           in (b', LilySequential (e':es'))

relativeSim :: LilyNote -> LilySimultaneous -> (LilyNote, LilySimultaneous)
relativeSim base (LilySimultaneous []) = (base, LilySimultaneous [])
relativeSim base (LilySimultaneous (e:es)) = let (b, e') = relativeExpr base e
                                                 (b', (LilySimultaneous es')) = relativeSim b (LilySimultaneous es)
                                             in (b', LilySimultaneous (e':es'))

isNote (Note (LilyNote _ _ _ _)) = True
isNote _ = False

-- isVoice (Seq _) = True

firstNote :: LilyExpr -> Maybe LilyNote
firstNote (Note n@(LilyNote _ _ _ _)) = Just n
firstNote (Seq (LilySequential (e:es))) = case (firstNote e) of
                                           (Just n) -> (Just n)
                                           Nothing -> firstNote (Seq (LilySequential es))
firstNote (Sim (LilySimultaneous (e:es))) = case (firstNote e) of
                                             (Just n) -> (Just n)
                                             Nothing -> firstNote (Sim (LilySimultaneous es))
firstNote (Chord (LilyChord (n:ns) d)) = Just n
firstNote (Voice (LilyVoice _ _ e)) = firstNote e
firstNote (Staff (LilyStaff _ _ e)) = firstNote e
firstNote (Rel (LilyRelative _ e)) = firstNote e
firstNote (Fix (LilyFixed _ e)) = firstNote e
firstNote (Trans (LilyTranspose _ _ e)) = firstNote e
-- firstNote _ = error "Couldn\'t find first note in expression"
firstNote _ = Nothing

relativeExpr :: LilyNote -> LilyExpr -> (LilyNote, LilyExpr)
relativeExpr base (Note n@(LilyNote _ _ _ _)) = let n' = relativeToAbsolute base n in (n', Note n')
relativeExpr base (Note n) = (base, Note n)
relativeExpr base (Seq s) = let (b, s') = relativeSeq base s in (b, Seq s')
relativeExpr base (Sim s) = let (b, s') = relativeSim base s in (b, Sim s')
relativeExpr base (Chord (LilyChord ns d)) = let ns' = map (relativeToAbsolute base) ns
                                                 b' = head ns'
                                             in (b', Chord $ LilyChord ns' d)
relativeExpr base (Voice (LilyVoice s c e)) = let (b, e') = relativeExpr base e
                                              in (b, Voice $ LilyVoice s c e')
relativeExpr base (Staff (LilyStaff s c e)) = let (b, e') = relativeExpr base e
                                              in (b, Staff $ LilyStaff s c e')
relativeExpr base (Trans (LilyTranspose n n' e)) = let (b, e') = relativeExpr base e
                                                   in (b, Trans $ LilyTranspose n n' e')
relativeExpr base (Rel (LilyRelative (Just (n, o)) e)) = let (b, e') = relativeExpr (LilyNote n o (1%4) []) e
                                                         in (base, e')
relativeExpr base (Rel (LilyRelative Nothing e)) = let (b, e') = relativeExpr p e
                                                       p = case firstNote e of
                                                            (Just n) -> n
                                                            Nothing -> error "Couldn\'t find first note in expression"
                                                   in (base, e')
relativeExpr base (Fix (LilyFixed _ e)) = (base, e) -- or not????
relativeExpr base (Tup (LilyTuplet r e)) = let (b', e') = relativeExpr base e
                                           in (b', Tup (LilyTuplet r e'))
relativeExpr base e = (base, e)

-- the 'default' note to use in absence of other information about octave number & note length etc.
defaultNote = LilyNote L_C (LilyOctave 0) (1%4) []

expandRel :: LilyExpr -> LilyExpr
expandRel e = let (n, e') = relativeExpr defaultNote e
              in e'

getDuration :: LilyNote -> LilyNoteDuration
getDuration (LilyNote _ _ d _) = d
getDuration (LilyRest d)       = d
getDuration (LilyFullRest d)   = d
getDuration (LilySkip d)       = d

setDuration :: LilyNote -> LilyNote -> LilyNote
setDuration b (LilyNote n o (-1) e) = LilyNote n o (getDuration b) e
setDuration b (LilyRest (-1)) = LilyRest (getDuration b)
setDuration b (LilyFullRest (-1)) = LilyFullRest (getDuration b)
setDuration b (LilySkip (-1)) = LilySkip (getDuration b)
setDuration _ c = c

setDurChord :: LilyNote -> LilyChord -> LilyChord
setDurChord b (LilyChord ns (-1)) = LilyChord ns (getDuration b)
setDurChord _ c = c

setDurSeq :: LilyNote -> LilySequential -> (LilyNote, LilySequential)
setDurSeq base (LilySequential []) = (base, LilySequential [])
setDurSeq base (LilySequential (e:es)) = let (b, e') = setDurExpr base e
                                             (b', (LilySequential es')) = setDurSeq b (LilySequential es)
                                         in (b', LilySequential (e':es'))

setDurSim :: LilyNote -> LilySimultaneous -> (LilyNote, LilySimultaneous)
setDurSim base (LilySimultaneous []) = (base, LilySimultaneous [])
setDurSim base (LilySimultaneous (e:es)) = let (b, e') = setDurExpr base e
                                               (b', (LilySimultaneous es')) = setDurSim b (LilySimultaneous es)
                                           in (b', LilySimultaneous (e':es'))

setDurExpr :: LilyNote -> LilyExpr -> (LilyNote, LilyExpr)
setDurExpr base (Note n) = let n' = setDuration base n in (n', Note n')
-- setDurExpr base (Note n@(LilyNote _ _ _ _)) = let n' = setDuration base n in (n', Note n')
-- setDurExpr base (Note n) = (base, Note n)
setDurExpr base (Seq s) = let (b, s') = setDurSeq base s in (b, Seq s')
setDurExpr base (Sim s) = let (b, s') = setDurSim base s in (b, Sim s')
setDurExpr base (Chord c) = let c' = setDurChord base c
                                extractDur (LilyChord _ d) = LilyNote L_C (LilyOctave 0) d []
                            in (extractDur c', Chord c')
setDurExpr base (Voice (LilyVoice s c e)) = let (b, e') = setDurExpr base e
                                            in (b, Voice $ LilyVoice s c e')
setDurExpr base (Staff (LilyStaff s c e)) = let (b, e') = setDurExpr base e
                                            in (b, Staff $ LilyStaff s c e')
setDurExpr base (Rel (LilyRelative r e)) = let (b, e') = setDurExpr base e
                                           in (b, Rel (LilyRelative r e'))
setDurExpr base (Fix (LilyFixed r e)) = let (b, e') = setDurExpr base e
                                        in (b, Fix (LilyFixed r e'))
setDurExpr base (Trans (LilyTranspose n n' e)) = let (b, e') = setDurExpr base e
                                                 in (b, Trans (LilyTranspose n n' e'))
setDurExpr base (Tup (LilyTuplet r e)) = let (b', e') = setDurExpr base e
                                         in (b', Tup (LilyTuplet r e'))
setDurExpr base e = (base, e)

expandDur :: LilyExpr -> LilyExpr
expandDur e = let (n, e') = setDurExpr defaultNote e
              in e'

mapNotes :: (LilyNote -> LilyNote) -> LilyExpr -> LilyExpr
mapNotes f (Note n) = Note (f n)
mapNotes f (Seq (LilySequential s)) = Seq (LilySequential (map (mapNotes f) s))
mapNotes f (Sim (LilySimultaneous s)) = Sim (LilySimultaneous (map (mapNotes f) s))
mapNotes f (Voice (LilyVoice s c e)) = Voice (LilyVoice s c (mapNotes f e))
mapNotes f (Staff (LilyStaff s c e)) = Staff (LilyStaff s c (mapNotes f e))
mapNotes f (Rel (LilyRelative n e)) = Rel (LilyRelative n (mapNotes f e))
mapNotes f (Trans (LilyTranspose a b e)) = Trans (LilyTranspose a b (mapNotes f e))
mapNotes f (Fix (LilyFixed n e)) = Fix (LilyFixed n (mapNotes f e))
mapNotes f (Chord (LilyChord c d)) = Chord (LilyChord (map f c) d)
mapNotes f (Tup (LilyTuplet r e)) = Tup (LilyTuplet r (mapNotes f e))
mapNotes f e = e

mapIdents :: (LilyIdentifier -> LilyExpr) -> LilyExpr -> LilyExpr
mapIdents f (Ident i) = f i
mapIdents f (Seq (LilySequential s)) = Seq (LilySequential (map (mapIdents f) s))
mapIdents f (Sim (LilySimultaneous s)) = Sim (LilySimultaneous (map (mapIdents f) s))
mapIdents f (Voice (LilyVoice s c e)) = Voice (LilyVoice s c (mapIdents f e))
mapIdents f (Staff (LilyStaff s c e)) = Staff (LilyStaff s c (mapIdents f e))
mapIdents f (Rel (LilyRelative n e)) = Rel (LilyRelative n (mapIdents f e))
mapIdents f (Trans (LilyTranspose a b e)) = Trans (LilyTranspose a b (mapIdents f e))
mapIdents f (Fix (LilyFixed n e)) = Fix (LilyFixed n (mapIdents f e))
mapIdents f (Tup (LilyTuplet r e)) = Tup (LilyTuplet r (mapIdents f e))
mapIdents f e = e

mapSeqs :: (LilySequential -> LilySequential) -> LilyExpr -> LilyExpr
mapSeqs f (Seq (LilySequential s)) = Seq (f (LilySequential s))
mapSeqs f (Sim (LilySimultaneous s)) = Sim (LilySimultaneous (map (mapSeqs f) s))
mapSeqs f (Voice (LilyVoice s c e)) = Voice (LilyVoice s c (mapSeqs f e))
mapSeqs f (Staff (LilyStaff s c e)) = Staff (LilyStaff s c (mapSeqs f e))
mapSeqs f (Rel (LilyRelative n e)) = Rel (LilyRelative n (mapSeqs f e))
mapSeqs f (Trans (LilyTranspose a b e)) = Trans (LilyTranspose a b (mapSeqs f e))
mapSeqs f (Fix (LilyFixed n e)) = Fix (LilyFixed n (mapSeqs f e))
mapSeqs f (Tup (LilyTuplet r e)) = Tup (LilyTuplet r (mapSeqs f e))
mapSeqs f e = e


eval :: LilyDict -> LilyIdentifier -> LilyExpr
eval d (LilyIdentifier s) = let d' = map dictToAssoc d
                            in case lookup s d' of
                                Nothing -> Ident (LilyIdentifier s)
                                (Just e) -> e

-- TODO: implement dictionaries/scoping properly
dictToAssoc :: LilyAssignment -> (String, LilyExpr)
dictToAssoc (LilyAssignment s e) = (s, e)
dictToAssoc (LilySymbAssignment s l e) = (s, e)
dictToAssoc (LilyOverride a) = dictToAssoc a
dictToAssoc (LilyOnce a) = dictToAssoc a

generateDict :: LilyFile -> LilyDict
generateDict = (map unwrap . filter isAssignment)
  where isAssignment (Assignment _) = True
        isAssignment _ = False
        unwrap (Assignment a) = a


-- -- TODO: implement recursive variable expansion
-- expandVariables :: LilyFile -> LilyFile
-- expandVariables file = map expandVariable file
  -- where dict = generateDict file
        -- expandVariable (Score e) = Score (mapIdents (eval dict) e)
        -- expandVariable x = x

mapExprs :: (LilyExpr -> LilyExpr) -> LilyFile -> LilyFile
mapExprs _ [] = []
mapExprs f ((Score e):es) = (Score (f e)):(mapExprs f es)
mapExprs f ((Assignment (LilyAssignment s e)):es) = (Assignment (LilyAssignment s (f e))):(mapExprs f es)
mapExprs f (e:es) = e : (mapExprs f es)

addToDict :: LilyDict -> LilyAssignment -> LilyDict
addToDict d a = a:d


-- expand variables 'recursively'
expandVarsRec :: LilyDict -> LilyFile -> LilyFile
expandVarsRec d ((Assignment a):xs) = let a' = expandVarsRecAss d a
                                          d' = addToDict d a'
                                      in (Assignment a'):(expandVarsRec d' xs)
expandVarsRec d ((Score e):xs) = let e' = mapIdents (eval d) e
                                 in (Score e'):(expandVarsRec d xs)
expandVarsRec d (x:xs) = x:(expandVarsRec d xs)
expandVarsRec _ [] = []

expandVarsRecAss :: LilyDict -> LilyAssignment -> LilyAssignment
expandVarsRecAss d (LilyAssignment s e) = LilyAssignment s (mapIdents (eval d) e)
expandVarsRecAss d (LilySymbAssignment s l e) = LilySymbAssignment s l (mapIdents (eval d) e)
expandVarsRecAss d (LilyOverride a) = LilyOverride (expandVarsRecAss d a)
expandVarsRecAss d (LilyOnce a) = LilyOnce (expandVarsRecAss d a)

expandVariables = expandVarsRec []
