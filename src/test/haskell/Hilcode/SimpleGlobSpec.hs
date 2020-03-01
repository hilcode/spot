module Hilcode.SimpleGlobSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec

import qualified Control.Exception
import qualified Data.Vector
import qualified TextShow

import           Hilcode.Nullable
import           Hilcode.SimpleGlob
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators
import           Hilcode.Util.Literals

propertyNullable :: Property
propertyNullable = property $ do
    (simpleGlob, simpleGlobNullable) <- Hedgehog.forAll generateSimpleGlobAndNullable
    assert $ nullable simpleGlob == simpleGlobNullable

spec :: Spec
spec = describe "SimpleGlob" $ do
    it "Eq SimpleGlob" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateSimpleGlob)
    it "Ord SimpleGlob" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd generateSimpleGlob)
    it "Nullable SimpleGlob" $
        HaskellWorks.Hspec.Hedgehog.require propertyNullable
    it "?[a-z]*a" $
        TextShow.showt simpleGlobOne   `shouldBe` "?[a-z]*a"
    it "?[a-z]*a|*[0-9]aa??" $
        TextShow.showt simpleGlobTwo   `shouldBe` "?[a-z]*a|*[0-9]aa??"
    it "?[a-z]*a|*[0-9]aa??|[0-9]*_?a?" $
        TextShow.showt simpleGlobThree `shouldBe` "?[a-z]*a|*[0-9]aa??|[0-9]*_?a?"
    it "xxxxa     ~ ?[a-z]*a            --> True" $
        matchSimpleGlob "xxxxa" simpleGlobOne
    it "xxxx8aa.. ~ ?[a-z]*a|*[0-9]aa?? --> True" $
        matchSimpleGlob "xxxx8aa.." simpleGlobTwo
    it "A GlobParts must have at least 1 GlobPart" $
        Control.Exception.evaluate (makeSimpleGlob Data.Vector.empty) `shouldThrow` errorCall "A 'SimpleGlob' _must_ have at least 1 'GlobParts'."
