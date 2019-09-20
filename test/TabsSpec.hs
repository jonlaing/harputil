module TabsSpec where

import           Action
import           Hole
import           Note
import           Tabs
import           Test.Hspec
import           Tuning

tuning :: Tuning
tuning =
  [ Hole 1 (Note 4 C) (Note 4 D)
  , Hole 2 (Note 4 E) (Note 4 G)
  , Hole 3 (Note 4 G) (Note 5 B)
  , Hole 4 (Note 5 C) (Note 5 D)
  , Hole 5 (Note 5 E) (Note 5 F)
  , Hole 6 (Note 5 G) (Note 6 A)
  , Hole 7 (Note 6 C) (Note 6 B)
  , Hole 8 (Note 6 E) (Note 6 D)
  , Hole 9 (Note 6 G) (Note 6 F)
  , Hole 10 (Note 7 C) (Note 7 A)
  ]

bluesRiffNotes :: [Note]
bluesRiffNotes = [Note 4 G, Note 5 C, Note 4 G, Note 5 Bb, Note 4 G]

bluesRiffTab :: Tabs
bluesRiffTab =
  [ HoleAction 2 Draw (Bend 0)
  , HoleAction 4 Blow (Bend 0)
  , HoleAction 2 Draw (Bend 0)
  , HoleAction 3 Draw (Bend 1)
  , HoleAction 2 Draw (Bend 0)
  ]

impossible :: [Note]
impossible = [Note 3 G, Note 4 C, Note 4 B]

spec :: Spec
spec =
  describe "parseTabs" $ do
    it "correctly parses a tab" $
      parseTabs tuning bluesRiffNotes `shouldBe` Just bluesRiffTab
    it "shows nothing for impossible riff" $
      parseTabs tuning impossible `shouldBe` Nothing
