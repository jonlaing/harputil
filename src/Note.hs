module Note
  ( NoteName(..)
  , Note(Note)
  , noteLevel
  , noteDifference
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

noteDifference :: Note -> Note -> Int
noteDifference a b = abs $ noteLevel a - noteLevel b

toNote :: Note -> Int -> Note
toNote (Note n note) i =
  Note
    (((i + fromEnum note) `div` 12) + n)
    (toEnum (mod (fromEnum note + i) 12))

parseNote :: String -> Maybe Note
parseNote "" = Nothing
parseNote s =
  case map T.unpack $ T.splitOn (T.pack ",") (T.pack s) of
    (noteName:noteLevel:_) ->
      Just $ Note (read noteLevel :: Int) (read noteName :: NoteName)
    _ -> Nothing

parseNotes :: String -> Maybe [Note]
parseNotes s = mapM (parseNote . T.unpack) $ T.splitOn (T.pack " ") (T.pack s)
