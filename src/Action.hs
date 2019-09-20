module Action
  ( Bend(..)
  , Breath(..)
  , HoleAction(..)
  , bendLevel
  , breath
  , action
  , findAction
  ) where

import           Data.Maybe (mapMaybe)
import           Hole       (Blow, Draw, Hole (Hole), HoleNumber, holeNumber)
import           Note       (Note, noteDifference, noteLevel)
import           Tuning     (Tuning)

newtype Bend =
  Bend Int
  deriving (Ord, Eq)

instance Show Bend where
  show (Bend i)
    | i < 0 = replicate (abs i) '^'
    | otherwise = replicate i '\''

data Breath
  = Blow
  | Draw
  deriving (Eq)

instance Show Breath where
  show Blow = "b"
  show Draw = "d"

data HoleAction =
  HoleAction HoleNumber Breath Bend
  deriving (Eq)

instance Show HoleAction where
  show (HoleAction n breath bend) = show n ++ show breath ++ show bend

-- the idea here is that the easiest hole to hit will be less than
-- a more difficult hole. This favors bends compared to overbends
instance Ord HoleAction where
  compare (HoleAction _ _ (Bend a)) (HoleAction _ _ (Bend b)) =
    compare (abs a) (abs b)

-- assumes the hole contains the note
bendLevel :: Note -> Hole -> Maybe Bend
bendLevel note (Hole _ blow draw)
  | note == blow || note == draw = Just $ Bend 0
  | otherwise =
    let level = noteLevel (max blow draw) - noteLevel note
     in if level > -2 && level < noteDifference blow draw
          then Just $ Bend level
          else Nothing

breath :: Note -> Hole -> Breath
breath note (Hole _ b d)
  | note == b = Blow
  | note == d = Draw
  | note < b && note > d = Blow
  | note > b && note < d = Draw
  | otherwise =
    if b > d
      then Draw
      else Blow

action :: Note -> Hole -> Maybe HoleAction
action note hole =
  case bendLevel note hole of
    Just b  -> Just $ HoleAction (holeNumber hole) (breath note hole) b
    Nothing -> Nothing

findAction :: Tuning -> Note -> Maybe HoleAction
findAction tuning note =
  case mapMaybe (action note) tuning of
    [] -> Nothing
    xs -> Just $ minimum xs
