module Action
  ( Bend
  , Breath
  , HoleAction
  , bendLevel
  , breath
  , action
  , findAction
  ) where

import           Hole   (Blow, Draw, Hole (Hole), HoleNumber, holeNumber)
import           Note   (Note, noteLevel)
import           Tuning (Tuning)

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
bendLevel :: Note -> Hole -> Bend
bendLevel note (Hole _ blow draw)
  | note == blow || note == draw = Bend 0
  | otherwise = Bend $ noteLevel (max blow draw) - noteLevel note

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

action :: Note -> Hole -> HoleAction
action note hole =
  HoleAction (holeNumber hole) (breath note hole) (bendLevel note hole)

findAction :: Tuning -> Note -> HoleAction
findAction tuning note = minimum $ map (action note) tuning
