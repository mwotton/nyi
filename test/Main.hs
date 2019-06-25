{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Debug.NYI
import Control.Exception(evaluate)
import Text.Show.Pretty(ppShow)
import Data.List(sort)

main = hspec $ do
  describe "nyi" $ do
    it "throws an exception" $ do
      let l = $nyi
      evaluate l `shouldThrow` anyException
    it "also lets you use todos" $ do
      let l = $(todo "end world hunger")
      evaluate l `shouldThrow` anyException
    it "can see where nyi has been used" $ do
      errors <- allNotImplemented
      putStrLn $ ppShow errors
      -- we will assume that the location is correct - annoying to have to update
      -- code to know about error locations.
      sort (fmap fst errors) `shouldBe` sort ["Not yet implemented", "end world hunger"]
