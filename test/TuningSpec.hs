module TuningSpec where

import qualified Data.Text  as T
import           Hole
import           Note
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

spec :: Spec
spec = do
  describe "tuning <-> tuning chart" $
    it "converts between tuning and tuning chart" $
    toTuning (Note 4 C) (toTuningChart tuning) `shouldBe` tuning
  describe "parsing" $ do
    describe "parseLine" $ do
      it "fails gracefully with no input" $
        parseLine (T.pack "") `shouldBe` Nothing
      it "fails gracefully with nonsense input" $
        parseLine (T.pack "asdfsd") `shouldBe` Nothing
