module Vector.SampleSpec where

import Vector.Sample

import Test.Hspec

spec :: Spec
spec = do
  describe "fromList and toList" $
    it "they are equivalent" $
      let a = [1,2,3] :: [Int]
      in a `shouldBe` toList(fromList a)

  describe "compareBytes" $ do
    it "compares the values not the pointer" $
      let a = fromList ([1,2,3] :: [Int])
          b = fromList ([1,2,3] :: [Int])
      in compareBytes a b `shouldBe` True

    it "compare different lists" $
      let a = fromList ([1,2,5] :: [Int])
          b = fromList ([1,2,3] :: [Int])
      in compareBytes a b `shouldBe` False

    it "compare different sized lists" $
      let a = fromList ([] :: [Int])
          b = fromList ([1,2,3] :: [Int])
      in compareBytes a b `shouldBe` False

  describe "map" $
    xit "applies a function to every element on the vector" $
      let a  = fromList ([1..3] :: [Int])
          a' = map' (+1) a
      in [2,3,4] `shouldBe` toList a'

  describe "concat" $ do
    it "concats two vectors" $
      let a = fromList ([1..3] :: [Int])
          b = fromList ([4..6] :: [Int])
          c = Vector.Sample.concat a b
      in toList c `shouldBe` [1..6]

    it "doesn't concat empty vectors" $
      let a = fromList ([1..3] :: [Int])
          b = fromList ([] :: [Int])
          c = Vector.Sample.concat a b
      in toList c `shouldBe` [1..3]



