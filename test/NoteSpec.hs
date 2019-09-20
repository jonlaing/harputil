module NoteSpec where

import           Note
import           Test.Hspec

spec :: Spec
spec = do
  describe "noteLevel" $
    it "should have a zero value" $ noteLevel (Note 0 Ab) `shouldBe` 0
  describe "toNote" $ do
    it "should count up by half steps from base note" $
      toNote (Note 0 Ab) 1 `shouldBe` Note 0 A
    it "should keep note the same at 0" $
      toNote (Note 4 G) 0 `shouldBe` Note 4 G
  describe "parseNote" $ do
    it "properly parses a note" $ parseNote "C,4" `shouldBe` Just (Note 4 C)
    it "fails gracefully for empty string" $ parseNote "" `shouldBe` Nothing
    it "fails gracefully for nonsense" $ parseNote "asdfsd" `shouldBe` Nothing
  describe "praseNotes" $ do
    it "can parse one note" $ parseNotes "C,4" `shouldBe` Just [Note 4 C]
    it "can parse more than one note" $
      parseNotes "C,4 A,4" `shouldBe` Just [Note 4 C, Note 4 A]
    it "fails gracefully for empty string" $ parseNotes "" `shouldBe` Nothing
    it "fails gracefully for nonsense" $ parseNotes "asdfdsf" `shouldBe` Nothing
