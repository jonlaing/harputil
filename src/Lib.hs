module Lib
  (
  ) where

import Hole (Hole(Hole))
import Note (Note(Note), NoteName(..))
import Tuning

richterTuningC = [
    Hole 1 (Note 4 C) (Note 4 D),
    Hole 2 (Note 4 E) (Note 4 G),
    Hole 3 (Note 4 G) (Note 5 B),
    Hole 4 (Note 5 C) (Note 5 D),
    Hole 5 (Note 5 E) (Note 5 F),
    Hole 6 (Note 5 G) (Note 6 A),
    Hole 7 (Note 6 C) (Note 6 B),
    Hole 8 (Note 6 E) (Note 6 D),
    Hole 9 (Note 6 G) (Note 6 F),
    Hole 10 (Note 7 C) (Note 7 A) ]