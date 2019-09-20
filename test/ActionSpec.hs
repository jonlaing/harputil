module ActionSpec where

import           Action
import           Hole
import           Note
import           Test.Hspec
import           Tuning

hole1 :: Hole
hole1 = Hole 1 (Note 4 C) (Note 4 D)

hole1Rev :: Hole
hole1Rev = Hole 1 (Note 4 D) (Note 4 C)

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
  describe "Bend" $ do
    describe "show" $ do
      it "shows no bend" $ show (Bend 0) `shouldBe` ""
      it "displays one bend" $ show (Bend 1) `shouldBe` "'"
      it "displays two bends" $ show (Bend 2) `shouldBe` "''"
      it "displays an overbend" $ show (Bend (-1)) `shouldBe` "^"
    describe "bendLevel" $ do
      it "shows no bend for natural blow note" $
        bendLevel (Note 4 C) hole1 `shouldBe` Just (Bend 0)
      it "shows no bend for natural draw note" $
        bendLevel (Note 4 D) hole1 `shouldBe` Just (Bend 0)
      it "shows half-step draw bend" $
        bendLevel (Note 4 Db) hole1 `shouldBe` Just (Bend 1)
      it "shows half-step blow bend" $
        bendLevel (Note 4 Db) hole1Rev `shouldBe` Just (Bend 1)
      it "shows half-step overblow" $
        bendLevel (Note 4 Eb) hole1 `shouldBe` Just (Bend (-1))
      it "shows half-step overdraw" $
        bendLevel (Note 4 Eb) hole1Rev `shouldBe` Just (Bend (-1))
      it "has nothing for draw bends more than hole difference" $
        bendLevel (Note 4 B) hole1 `shouldBe` Nothing
      it "has nothing for blow bends more than hold difference" $
        bendLevel (Note 4 B) hole1Rev `shouldBe` Nothing
      it "has nothing for overblows greater than a half step" $
        bendLevel (Note 4 E) hole1 `shouldBe` Nothing
      it "has nothing for overdraws greater than a half step" $
        bendLevel (Note 4 E) hole1Rev `shouldBe` Nothing
  describe "Breath" $ do
    describe "show" $ do
      it "displays blow" $ show Blow `shouldBe` "b"
      it "displays draw" $ show Draw `shouldBe` "d"
    describe "breath" $ do
      it "shows blow for natural note" $ breath (Note 4 C) hole1 `shouldBe` Blow
      it "shows draw for natural note" $ breath (Note 4 D) hole1 `shouldBe` Draw
      it "shows draw for draw bent note" $
        breath (Note 4 Db) hole1 `shouldBe` Draw
      it "shows blow for blow bent note" $
        breath (Note 4 Db) hole1Rev `shouldBe` Blow
      it "shows blow for overblow note" $
        breath (Note 4 Eb) hole1 `shouldBe` Blow
      it "shows draw for overdrawn note" $
        breath (Note 4 Eb) hole1Rev `shouldBe` Draw
  describe "HoleAction" $ do
    describe "show" $ do
      it "shows a natural blow note" $
        show (HoleAction 1 Blow (Bend 0)) `shouldBe` "1b"
      it "shows a natural draw note" $
        show (HoleAction 1 Draw (Bend 0)) `shouldBe` "1d"
      it "shows a half-step blow bend" $
        show (HoleAction 1 Blow (Bend 1)) `shouldBe` "1b'"
      it "shows a half-step draw bend" $
        show (HoleAction 1 Draw (Bend 1)) `shouldBe` "1d'"
      it "shows an overblow" $
        show (HoleAction 1 Blow (Bend (-1))) `shouldBe` "1b^"
      it "shows an overdraw" $
        show (HoleAction 1 Draw (Bend (-1))) `shouldBe` "1d^"
    describe "ord" $ do
      it "compares natural less than bent notes" $
        HoleAction 1 Draw (Bend 0) < HoleAction 2 Blow (Bend 1) `shouldBe` True
      it "compares single bends less than double bends" $
        HoleAction 1 Blow (Bend 1) < HoleAction 2 Draw (Bend 2) `shouldBe` True
      it "compares bends and overbends by absolute values" $
        HoleAction 1 Blow (Bend (-1)) <
        HoleAction 2 Draw (Bend 2) `shouldBe` True
    describe "action" $ do
      it "finds natural blow note" $
        action (Note 4 C) hole1 `shouldBe` (Just $ HoleAction 1 Blow (Bend 0))
      it "finds natural draw note" $
        action (Note 4 D) hole1 `shouldBe` (Just $ HoleAction 1 Draw (Bend 0))
      it "finds draw bend note" $
        action (Note 4 Db) hole1 `shouldBe` (Just $ HoleAction 1 Draw (Bend 1))
      it "finds overblow note" $
        action (Note 4 Eb) hole1 `shouldBe`
        (Just $ HoleAction 1 Blow (Bend (-1)))
      it "doesn't find notes below range" $
        action (Note 4 B) hole1 `shouldBe` Nothing
      it "doesn't find notes above range" $
        action (Note 4 E) hole1 `shouldBe` Nothing
    describe "findAction" $ do
      it "finds natural blow note" $
        findAction tuning (Note 4 C) `shouldBe`
        (Just $ HoleAction 1 Blow (Bend 0))
      it "finds natural draw note" $
        findAction tuning (Note 4 D) `shouldBe`
        (Just $ HoleAction 1 Draw (Bend 0))
      it "finds draw bend note" $
        findAction tuning (Note 4 Db) `shouldBe`
        (Just $ HoleAction 1 Draw (Bend 1))
      it "finds overblow note" $
        findAction tuning (Note 4 Eb) `shouldBe`
        (Just $ HoleAction 1 Blow (Bend (-1)))
      it "doesn't find notes below range" $
        action (Note 4 B) hole1 `shouldBe` Nothing
      it "doesn't find notes above range" $
        action (Note 7 D) hole1 `shouldBe` Nothing
