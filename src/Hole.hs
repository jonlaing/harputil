module Hole
  ( Draw
  , Blow
  , Hole(Hole)
  , HoleNumber
  , holeNumber
  ) where

import           Note (Note)

type Draw = Note

type Blow = Note

type HoleNumber = Int

data Hole =
  Hole HoleNumber Blow Draw
  deriving (Show, Eq)

holeNumber :: Hole -> HoleNumber
holeNumber (Hole n _ _) = n
