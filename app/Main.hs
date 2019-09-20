module Main where

import           Action             (findAction)
import qualified Data.Text          as T
import           Lib
import           Note               (Note (Note), NoteName (..), parseNote,
                                     parseNotes)
import           System.Environment
import           System.IO
import           Tabs               (Tabs)
import           Tuning             (TuningChart, parseTuning)

makeTab :: String -> String -> String -> Maybe String
makeTab baseNoteString tuningString noteString = do
  baseNote <- parseNote baseNoteString
  tuning <- parseTuning baseNote tuningString
  notes <- parseNotes noteString
  let tab = mapM (findAction tuning) notes
  return $ foldl (\acc t -> acc ++ show t ++ " ") "" tab

richter :: String
richter = "0,2\n4,7\n7,11\n12,14\n16,17\n19,21\n24,23\n28,26\n31,29\n36,33"

main = do
  args <- getArgs
  let (tuningFile:baseNoteString:noteString:_) = args
  handle <- openFile ("tunings/" ++ tuningFile ++ ".txt") ReadMode
  tuningString <- hGetContents handle
  case makeTab baseNoteString tuningString noteString of
    Just tab -> putStrLn tab
    Nothing  -> putStrLn "Error making tab"
