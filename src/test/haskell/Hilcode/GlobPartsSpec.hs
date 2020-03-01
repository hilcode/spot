module Hilcode.GlobPartsSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec

import qualified Control.Exception
import qualified Data.Vector
import qualified TextShow

import           Hilcode.CharSet
import           Hilcode.GlobPart
import           Hilcode.GlobParts
import           Hilcode.Nullable
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators
import           Hilcode.Util.Literals

propertyNullable :: Property
propertyNullable = property $ do
    (globParts, globPartsNullable) <- Hedgehog.forAll generateGlobPartsAndNullable
    assert $ nullable globParts == globPartsNullable

propertyMatchGlobParts :: Property
propertyMatchGlobParts = property $ do
    (globParts, text) <- Hedgehog.forAll generateGlobPartsAndText
    assert $ matchGlobParts text globParts

spec :: Spec
spec = describe "GlobParts" $ do
    it "Eq GlobParts" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateGlobParts)
    it "Ord GlobParts" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd generateGlobParts)
    it "Nullable GlobParts" $
        HaskellWorks.Hspec.Hedgehog.require propertyNullable
    it "match GlobParts" $
        HaskellWorks.Hspec.Hedgehog.require propertyMatchGlobParts
    it "*" $
        TextShow.showt (makeGlobParts (Data.Vector.singleton Many))                                                    == "*"
    it "?*" $
        TextShow.showt (makeGlobParts (Data.Vector.fromList [Any, Many]))                                              == "?*"
    it "?*a" $
        TextShow.showt (makeGlobParts (Data.Vector.fromList [Any, Many, Single 'a']))                                  == "?*a"
    it "?*a[0-9]" $
        TextShow.showt (makeGlobParts (Data.Vector.fromList [Any, Many, Single 'a', Range $ includeRange ('0', '9')])) == "?*a[0-9]"
    it "?[a-z]*a" $
        TextShow.showt globPartsA                                                                                      == "?[a-z]*a"
    it "*[0-9]aa??" $
        TextShow.showt globPartsB                                                                                      == "*[0-9]aa??"
    it "[0-9]*_?a?" $
        TextShow.showt globPartsC                                                                                      == "[0-9]*_?a?"
    it "A SimpleGlob must have at least 1 GlobParts" $
        Control.Exception.evaluate (makeGlobParts Data.Vector.empty) `shouldThrow` errorCall "A 'GlobParts' _must_ have at least 1 'GlobPart'."
