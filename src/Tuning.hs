module Tuning
  ( Tuning
  , TuningChart
  , toTuningChart
  , toTuning
  , parseTuning
  ) where

import           Hole (Hole (Hole))
import           Note (Note, noteLevel, toNote)
import qualified Data.Text as T

type Tuning = [Hole]

type TuningChart = [(Int, Int)]

toTuningChart :: Tuning -> TuningChart
toTuningChart tuning =
  map
    (\(Hole _ blow draw) -> (noteLevel blow - root, noteLevel draw - root))
    tuning
  where
    (Hole _ rootNote _) = head tuning
    root = noteLevel rootNote

toTuning :: Note -> TuningChart -> Tuning
toTuning root chart =
  zipWith (\(b, d) i -> Hole i (toNote root b) (toNote root d)) chart [1 ..]

parseLine :: T.Text -> (Int, Int)
parseLine line = (a, b)
  where
    (a:b:_) = map (read . T.unpack) $ T.splitOn (T.pack ",") line

parseTuningChart :: String -> TuningChart
parseTuningChart lines = map parseLine $ T.lines $ T.pack lines

parseTuning :: Note -> String -> Tuning
parseTuning note = toTuning note . parseTuningChart