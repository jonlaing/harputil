module Note
  ( NoteName(..)
  , Note(Note)
  , noteLevel
  , toNote
  , parseNote
  , parseNotes
  ) where

import qualified Data.Text as T
import           Text.Read

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
  deriving (Show, Read, Eq, Ord, Enum)

data Note =
  Note Int NoteName
  deriving (Show, Eq, Ord)

noteLevel :: Note -> Int
noteLevel (Note n note) = fromEnum note + (12 * n)

toNote :: Note -> Int -> Note
toNote (Note n note) i =
  Note (((i + n) `div` 12) + fromEnum note) (toEnum (mod (n + i) 12))

parseNote :: String -> Note
parseNote s = Note (read noteLevel :: Int) (read noteName :: NoteName)
  where
    (noteName:noteLevel:_) = map T.unpack $ T.splitOn (T.pack ",") (T.pack s)

parseNotes :: String -> [Note]
parseNotes s = map (parseNote . T.unpack) $ T.splitOn (T.pack " ") (T.pack s)
