module Tabs
  ( Tabs(..)
  , parseTabs
  ) where

import           Action (HoleAction, findAction)
import           Note   (Note)
import           Tuning (Tuning)

type Tabs = [HoleAction]

parseTabs :: Tuning -> [Note] -> Maybe Tabs
parseTabs tuning = mapM (findAction tuning)
