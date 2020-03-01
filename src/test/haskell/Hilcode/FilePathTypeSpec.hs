module Hilcode.FilePathTypeSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           Test.Hspec

import qualified TextShow

import           Hilcode.FilePathType
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators

spec :: Spec
spec = describe "FilePathType" $ do
    it "Eq FilePathType" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateFilePathType)
    it "Ord FilePathType" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd generateFilePathType)
    it "Show Relative == 'Relative'" $
        TextShow.showt Relative `shouldBe` "Relative"
    it "Show Absolute == 'Absolute'" $
        TextShow.showt Absolute `shouldBe` "Absolute"
