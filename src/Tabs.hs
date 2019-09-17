module Tabs
  ( Tabs
  , parseTabs
  ) where

import           Action (HoleAction, findAction)
import           Note   (Note)
import           Tuning (Tuning)

type Tabs = [HoleAction]

parseTabs :: Tuning -> [Note] -> Tabs
parseTabs tuning = map $ findAction tuning
