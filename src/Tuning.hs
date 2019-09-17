module Tuning
  ( Tuning
  , TuningChart
  , toTuningChart
  , toTuning
  ) where

import           Hole (Hole (Hole))
import           Note (Note, noteLevel, toNote)

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
