module Music where

import           Control.Arrow
import qualified Data.EventList.Relative.TimeBody as EL
import           Data.List
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as FE
import qualified Sound.MIDI.File.Load             as FL
import qualified Sound.MIDI.Message.Channel.Voice as MCV
import qualified Sound.MIDI.Message.Channel       as MC

type Time = Double

data Beat
  = F1
  | F2
  | F3
  | F4
  deriving (Show, Eq, Ord, Enum)

newtype Note = Note { note :: (Time, Beat) }
        deriving (Show, Eq, Ord)

-- Code

midiLoadFile :: FilePath -> IO F.T
midiLoadFile
  = FL.fromFile

fetchNoteEvents :: [(F.ElapsedTime, FE.T)] -> [(F.ElapsedTime, MCV.T)]
fetchNoteEvents fe
  = map (second toMCV) (filter (isMCV . snd)
      (map (second (MC.messageBody . eventToChannelT)) fe))

maxNotes :: [MCV.T] -> Int
maxNotes a =
  maximum (map MC.fromPitch (pitches a))

minNotes :: [MCV.T] -> Int
minNotes a =
  minimum (map MC.fromPitch (pitches a))

pitches :: [MCV.T] -> [MC.Pitch]
pitches = map pitches'
  where
    pitches' (MCV.NoteOn p _) = p
    pitches' (MCV.NoteOff p _) = p

toMCV :: MC.Body -> MCV.T
toMCV (MC.Voice t) = t

isMCV :: MC.Body -> Bool
isMCV (MC.Voice _) = True
isMCV _            = False

eventToChannelT :: FE.T -> MC.T
eventToChannelT (FE.MIDIEvent t)
  = t
eventToChannelT p
  = error $ show p

getMidiEvents :: [(F.ElapsedTime, FE.T)] -> [(F.ElapsedTime, FE.T)]
getMidiEvents
  = filter (isMIDIEvent . snd)

isMIDIEvent :: FE.T -> Bool
isMIDIEvent (FE.MIDIEvent _) = True
isMIDIEvent _                = False

midiGetEvents :: [F.Track] -> [(F.ElapsedTime, FE.T)]
midiGetEvents tr
  = sort (concatMap EL.toPairList tr)

midiGetTracks :: F.T -> [F.Track]
midiGetTracks (F.Cons _ _ tracks)
  = tracks

midiFilterNoteMessage :: [MCV.T] -> [MCV.T]
midiFilterNoteMessage
  = filter noteChange

noteChange :: MCV.T -> Bool
noteChange (MCV.NoteOn _ _)  = True
noteChange (MCV.NoteOff _ _) = True
noteChange _                 = False

data Lol
  = Lel Int
  | Lul Int
  deriving (Show)
