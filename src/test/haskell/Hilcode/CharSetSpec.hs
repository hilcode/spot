module Hilcode.CharSetSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec
import qualified Hedgehog.Gen

import qualified Data.Char
import qualified Data.Foldable
import qualified TextShow

import           Hilcode.CharSet
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators
import           Hilcode.Util.Literals

propertyEmptyContains :: Property
propertyEmptyContains = property $ do
    ch <- Hedgehog.forAll Hedgehog.Gen.latin1
    footnote $ "Empty CharSet contains " <> show ch
    assertNot (includeNone `contains` ch)

propertySingletonSize :: Property
propertySingletonSize = property $ do
    ch <- Hedgehog.forAll Hedgehog.Gen.latin1
    footnote $ "Failing CharSet: CharSet " <> show ch
    let charSet = include ch in do
        assert (size charSet == 1)
        assert (charSet `contains` ch)
        assertNot (charSet `contains` succ ch)

propertyRangeSize ::Property
propertyRangeSize = property $ do
    (lower, upper) <- Hedgehog.forAll generateLowerUpper
    let charSet = includeRange (lower, upper) in do
        footnote $ "Failing CharSet: " <> show charSet
        assert (size charSet == (Data.Char.ord upper - Data.Char.ord lower + 1))
        assert (Data.Foldable.foldl' (&&) True (contains charSet <$> [lower..upper]))

spec :: Spec
spec = describe "CharSet" $ do
    it "Eq CharSet" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateCharSet)
    it "Ord CharSet" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd generateCharSet)
    it "propertyEmptyContains" $
        HaskellWorks.Hspec.Hedgehog.require propertyEmptyContains
    it "propertySingletonSize" $
        HaskellWorks.Hspec.Hedgehog.require propertySingletonSize
    it "propertyRangeSize" $
        HaskellWorks.Hspec.Hedgehog.require propertyRangeSize
    it "size empty" $
        size includeNone `shouldBe` 0
    it "size a-z" $
        size charSetLettersLowercase                                                         `shouldBe` 26
    it "{a} <> {z}" $
        size (include 'a' <> include 'z')                                                `shouldBe` 2
    it "{} == []" $
        TextShow.showt (mempty :: CharSet)                                                   `shouldBe` "[]"
    it "{a} == [a]" $
        TextShow.showt (include 'a')                                                       `shouldBe` "[a]"
    it "{a,b} == [ab]" $
        TextShow.showt (include 'a' <> include 'b')                                      `shouldBe` "[ab]"
    it "{a-c} == [a-c]" $
        TextShow.showt (includeRange ('a', 'c'))                                                    `shouldBe` "[a-c]"
    it "{a-df} == [a-df]" $
        TextShow.showt (includeRange ('a', 'd') <> include 'f')                                   `shouldBe` "[a-df]"
    it "{ac-e} == [ac-e]" $
        TextShow.showt (include 'a' <> includeRange ('c', 'e'))                                   `shouldBe` "[ac-e]"
    it "{a-ce} == [a-ce]" $
        TextShow.showt (includeRange ('a', 'c') <> include 'e')                                   `shouldBe` "[a-ce]"
    it "{a-ce-h} == [a-ce-h]" $
        TextShow.showt (includeRange ('a', 'c') <> includeRange ('e', 'h'))                                `shouldBe` "[a-ce-h]"
    it "{a-ce-h} == [a-ce-h]" $
        TextShow.showt (Data.Foldable.foldl' (<>) mempty [includeRange ('a', 'c'), mempty, includeRange ('e', 'h')]) `shouldBe` "[a-ce-h]"
