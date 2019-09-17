module Main where

import qualified Data.Text as T
import           Lib
import           Note      (Note (Note), NoteName (..))
import           System.IO
import           Tuning    (TuningChart, toTuning)

parseLine :: T.Text -> (Int, Int)
parseLine line = (a, b)
  where
    (a:b:_) = map (read . T.unpack) $ T.splitOn (T.pack ",") line

parse :: String -> TuningChart
parse lines = map parseLine $ T.lines $ T.pack lines

main = do
  handle <- openFile "tunings/richter.txt" ReadMode
  contents <- hGetContents handle
  let chart = parse contents
  let tuning = toTuning (Note 4 C) chart
  print tuning
  return ""
