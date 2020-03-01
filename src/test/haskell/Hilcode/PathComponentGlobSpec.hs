module Hilcode.PathComponentGlobSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec
import qualified Hedgehog.Gen
import qualified Hedgehog.Range

import qualified Data.Text
import qualified TextShow

import           Hilcode.Nullable
import           Hilcode.PathComponentGlob
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators
import           Hilcode.Util.Literals

propertyNullable :: Property
propertyNullable = property $ do
    (pathComponentGlob, pathComponentGlobNullable) <- Hedgehog.forAll generatePathComponentGlobAndNullable
    assert $ nullable pathComponentGlob == pathComponentGlobNullable

propertyManyDirectoriesPartMatcher :: Property
propertyManyDirectoriesPartMatcher = property $ do
    text <- Hedgehog.forAll (Hedgehog.Gen.list (Hedgehog.Range.constant 0 10) Hedgehog.Gen.latin1)
    assert $ partMatcher (Data.Text.pack text) ManyDirectories

propertyPathComponentGlobPartMatcher :: Property
propertyPathComponentGlobPartMatcher = property $ do
    (simpleGlob, text) <- Hedgehog.forAll generateSimpleGlobAndText
    assert $ partMatcher text (PathComponentGlob simpleGlob)

propertyPathComponentGlobShowT :: Property
propertyPathComponentGlobShowT = property $ do
    pathComponentGlob <- Hedgehog.forAll generatePathComponentGlob
    case pathComponentGlob of
        ManyDirectories              -> assert $ TextShow.showt pathComponentGlob == "**"
        PathComponentGlob simpleGlob -> assert $ TextShow.showt pathComponentGlob == TextShow.showt simpleGlob

spec :: Spec
spec = describe "PathComponentGlob" $ do
    it "Eq PathComponentGlob" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generatePathComponentGlob)
    it "Ord PathComponentGlob" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd generatePathComponentGlob)
    it "Nullable PathComponentGlob" $
        HaskellWorks.Hspec.Hedgehog.require propertyNullable
    it "match ManyDirectories" $
        HaskellWorks.Hspec.Hedgehog.require propertyManyDirectoriesPartMatcher
    it "match PathComponentGlob" $
        HaskellWorks.Hspec.Hedgehog.require propertyPathComponentGlobPartMatcher
    it "ShowT PathComponentGlob" $
        HaskellWorks.Hspec.Hedgehog.require propertyPathComponentGlobShowT
    it "**" $
        TextShow.showt manyDirectories        `shouldBe` "**"
    it "?[a-z]*a" $
        TextShow.showt pathComponentGlobOne   `shouldBe` "?[a-z]*a"
    it "?[a-z]*a|*[0-9]aa??" $
        TextShow.showt pathComponentGlobTwo   `shouldBe` "?[a-z]*a|*[0-9]aa??"
    it "?[a-z]*a|*[0-9]aa??|[0-9]*_?a?" $
        TextShow.showt pathComponentGlobThree `shouldBe` "?[a-z]*a|*[0-9]aa??|[0-9]*_?a?"
