module Main where

import           Action             (findAction)
import qualified Data.Text          as T
import           Lib
import           Note               (Note (Note), NoteName (..), parseNotes)
import           System.Environment
import           System.IO
import           Tuning             (TuningChart, parseTuning)

main = do
  args <- getArgs
  let (tuningFile:noteString:_) = args
  handle <- openFile ("tunings/" ++ tuningFile ++ ".txt") ReadMode
  contents <- hGetContents handle
  let tuning = parseTuning (Note 4 C) contents
  let notes = parseNotes noteString
  let tab = map (findAction tuning) notes
  print tab
  return ""
