module Note
  ( NoteName(..)
  , Note(Note)
  , noteLevel
  , toNote
  ) where

data NoteName
  = Ab
  | A
  | Bb
  | B
  | C
  | Db
  | D
  | Eb
  | E
  | F
  | Gb
  | G
  deriving (Show, Eq, Ord, Enum)

data Note =
  Note Int NoteName
  deriving (Show, Eq, Ord)

noteLevel :: Note -> Int
noteLevel (Note n note) = fromEnum note + (12 * n)

toNote :: Note -> Int -> Note
toNote (Note n note) i =
  Note (((i + n) `div` 12) + fromEnum note) (toEnum (mod (n + i) 12))
