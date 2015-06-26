-- by Stephen Lavelle --
-- http://www.maths.tcd.ie/~icecube --

module Miditest where
import Codec.Midi
import Data.List

type Note = Int
type Melody = [Note]
type MidiEvent = (Ticks, Message)


midiSkeleton :: Track Ticks -> Midi
midiSkeleton mel =  Midi {
         fileType = MultiTrack, 
         timeDiv = TicksPerBeat 480, 
         tracks = [
          [
           (0,ChannelPrefix 0),
           (0,TrackName " Grand Piano  "),
           (0,InstrumentName "GM Device  1"),
           (0,TimeSignature 4 2 24 8),
           (0,KeySignature 0 0)
          ]
          ++
          mel
          ++
          [
           (0,TrackEnd)
          ]
         ]
       }  


keydown :: Note -> MidiEvent
keydown k =  (0,NoteOn {channel = 0, key = k, velocity = 80})

keyup :: Note -> MidiEvent
keyup k =  (480,NoteOn {channel = 0, key = k, velocity = 0})

playnote :: Note -> Track Ticks
playnote k = [ keydown k, keyup k]
 

createMidi :: FilePath -> Melody -> IO()
createMidi f notes = exportFile  f $ midiSkeleton $ concat $ map  playnote notes
