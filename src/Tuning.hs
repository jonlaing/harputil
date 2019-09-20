module Tuning
  ( Tuning
  , TuningChart
  , toTuningChart
  , toTuning
  , parseTuning
  , parseLine
  ) where

import qualified Data.Text as T
import           Hole      (Hole (Hole))
import           Note      (Note, noteLevel, toNote)

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

parseLine :: T.Text -> Maybe (Int, Int)
parseLine line =
  case map (read . T.unpack) $ T.splitOn (T.pack ",") line of
    (a:b:_) -> Just (a, b)
    _       -> Nothing

parseTuningChart :: String -> Maybe TuningChart
parseTuningChart ""    = Nothing
parseTuningChart lines = mapM parseLine $ T.lines $ T.pack lines

parseTuning :: Note -> String -> Maybe Tuning
parseTuning note lines = toTuning note <$> parseTuningChart lines
