module Main where

import LilyParse
import Text.Parsec.String
import LilyConvert

import Music (mapMusic, mapPhrase, noteToSound, Metronome(..))
import Tuning
import Shortcuts

import Output
import Csound.Patch

import System.IO
import Control.Monad
import System.Environment
import System.Exit
import System.Console.GetOpt    

data Flag = File String
          | Speed String
          | Freq String
          | Tuning String
          | Instr String
          | Help
          deriving (Eq,Ord,Show)

flags = [Option ['w'] ["write"]     (ReqArg File "file") "Write a .wav file (defaults to playing sounds in real-time)",
         Option ['t'] ["tuning"]    (ReqArg Tuning "name") ("Use named tuning system t (defaults to equal temperament). \n\t\tAcceptable tuning systems are: " ++ tuningSystems ++ "\n\t\t where \"tet\" is equal temperament and \"..mt\" is a meantone."),
         Option ['f'] ["freq"]      (ReqArg Freq "freq") "Use frequency f (in Hz) for treble A (defaults to 440)",
         Option ['m'] ["metronome"] (ReqArg Speed "speed") "Use metronome mark s (defaults to 240)",
         Option ['i'] ["instrument"] (ReqArg Instr "instr") ("Use this instrument (defaults to harpsichord). \n\t\tAcceptable instruments are: " ++ instruments),
         Option ['h'] ["help"]      (NoArg Help) "Print this help message."]

parseArgs argv = case getOpt Permute flags argv of
                  (args,fs,[]) -> do let files = if null fs
                                                 then ["-"]
                                                 else fs
                                     if Help `elem` args
                                     then do hPutStrLn stderr (usageInfo header flags)
                                             exitWith ExitSuccess
                                     else return (args, files)
                  (_,_,errs)    -> do hPutStrLn stderr (concat errs ++ usageInfo header flags)
                                      exitWith (ExitFailure 1)
        where header = "Usage: readlily [options] file.ly"

tuningSystems = 
  foldl (\a b -> a ++ ", " ++ b) "equal" $
  ["qcmt"      ,
   "scmt",
   "tcmt",
   "pythag"    ,
   "septimal"  ,
   "kleismic"  ,
   "inverted"  ,
   "tet5"      ,
   "tet7"      ,
   "tet12"     ,
   "tet19"     ,
   "tet31"     ,
   "tet53"]

getTuning f ((Tuning t):as) = case t of
  "equal"       -> equal (a, f)
  "qcmt"        -> qcmeantone (a, f)
  "scmt"        -> scmeantone (a, f)
  "tcmt"        -> tcmeantone (a, f)
  "inverted"    -> inverted (a, f)
  "pythag"      -> pythagorean (a, f)
  "septimal"    -> septimal (a, f)
  "kleismic"    -> kleismic (a, f)
  "tet5"        -> synTET5  (a, f)   
  "tet7"        -> synTET7  (a, f)
  "tet12"       -> synTET12 (a, f)
  "tet19"       -> synTET19 (a, f)
  "tet31"       -> synTET31 (a, f)
  "tet53"       -> synTET53 (a, f)
  otherwise     -> equal (a, f)
getTuning f (a:as) = getTuning f as
getTuning f [] = equal (a, f)

getSpeed ((Speed s):as) = Metronome (read s)
getSpeed (a:as) = getSpeed as
getSpeed [] = Metronome 240

getFreq ((Freq f):as) = freq (read f)
getFreq (a:as) = getFreq as
getFreq [] = freq 440

doParse :: String -> IO LilyFile
doParse fileName = parseFromFile parseLily fileName >>= either report return
    where report err = do hPutStrLn stderr $ "Error: " ++ show err
                          exitFailure

instruments =
  foldl (\a b -> a ++ ", " ++ b) "harpsichord" $
  [ "piano"     ,
    "organ"     ,
    "choir"     ,
    "pad"       ,
    "guitar"    ,
    "flute"     ,
    "clarinet"  ,
    "horn"      ,
    "violin"    ,
    "oboe"]

getInstr ((Instr i):as) = case i of
  "harpsichord" -> harpsichord
  "piano"       -> fmPiano
  "organ"       -> cathedralOrgan
  "choir"       -> choirA
  "pad"         -> overtonePad
  "guitar"      -> guitar
  "flute"       -> flute
  "clarinet"    -> bassClarinet
  "horn"        -> frenchHorn
  "violin"      -> soloSharc shViolin -- ????
  "oboe"        -> soloSharc shOboe
  otherwise     -> harpsichord
getInstr (a:as) = getInstr as
getInstr [] = harpsichord

getDest ((File f):as) = Just f
getDest (a:as) = getDest as
getDest [] = Nothing

main :: IO ()
main = do
  (args, files) <- getArgs >>= parseArgs

  lilycontents <- doParse (head files)
  let music = (getScore . expandVariables) lilycontents
      m = getSpeed args
      f = getFreq args
      t = getTuning f args
      i = getInstr args
      d = getDest args
      sounds = mapMusic (mapPhrase (noteToSound t m)) music
  case d of
   (Just f) -> writeCsound f i sounds
   Nothing  -> playCsound i sounds
  exitWith ExitSuccess







