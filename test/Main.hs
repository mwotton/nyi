{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
import Test.Hspec
import Debug.NYI(allNotImplemented)
import Control.Exception(evaluate)
import Data.List(sort)
import A(unimplemented)
import B(todowithmsg)

main :: IO ()
main = hspec $ do
  describe "nyi" $ do
    it "throws an exception" $ do
      evaluate unimplemented `shouldThrow` anyException
    it "also lets you use todos" $ do
      evaluate todowithmsg `shouldThrow` anyException
    it "can see where nyi has been used" $ do
      let errors = $allNotImplemented
      -- we will assume that the location is correct - annoying to have to update
      -- code to know about error locations.
      sort (fmap fst errors) `shouldBe` sort ["Not yet implemented", "can we escape", "end world hunger"]
