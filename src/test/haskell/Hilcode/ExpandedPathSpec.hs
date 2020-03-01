module Hilcode.ExpandedPathSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec

import qualified Data.Text
import qualified System.Path
import qualified TextShow

import           Hilcode.ExpandedPath
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators
import           Hilcode.Util.Literals

propertyExpandedPathRoundTrip :: Property
propertyExpandedPathRoundTrip = property $ do
    expandedPath <- Hedgehog.forAll generateExpandedPath
    footnote $ "Failing ExpandedPath: '" <> show expandedPath <> "'"
    case expandedPath of
        AbsoluteDirectory _ ->
            assert (toExpandedPath (System.Path.absDir $ Data.Text.unpack (TextShow.showt expandedPath)) == expandedPath)
        RelativeDirectory _ ->
            assert (toExpandedPath (System.Path.relDir $ Data.Text.unpack (TextShow.showt expandedPath)) == expandedPath)
        AbsoluteFile _ ->
            assert (toExpandedPath (System.Path.absFile $ Data.Text.unpack (TextShow.showt expandedPath)) == expandedPath)
        RelativeFile _ ->
            assert (toExpandedPath (System.Path.relFile $ Data.Text.unpack (TextShow.showt expandedPath)) == expandedPath)

propertyExpandedPathEndsWithSlashOnlyIfDirectory :: Property
propertyExpandedPathEndsWithSlashOnlyIfDirectory = property $ do
    expandedPath <- Hedgehog.forAll generateExpandedPath
    footnote $ "Failing ExpandedPath: '" <> show expandedPath <> "'"
    case expandedPath of
        AbsoluteDirectory _ ->
            assert ("/" `Data.Text.isSuffixOf` TextShow.showt expandedPath)
        RelativeDirectory _ ->
            assert ("/" `Data.Text.isSuffixOf` TextShow.showt expandedPath)
        AbsoluteFile _ ->
            assertNot ("/" `Data.Text.isSuffixOf` TextShow.showt expandedPath)
        RelativeFile _ ->
            assertNot ("/" `Data.Text.isSuffixOf` TextShow.showt expandedPath)

propertyExpandedPathShowMustNotContainDoubleSlash :: Property
propertyExpandedPathShowMustNotContainDoubleSlash = property $ do
    expandedPath <- Hedgehog.forAll generateExpandedPath
    footnote $ "Failing ExpandedPath: '" <> show expandedPath <> "'"
    assertNot ("//" `Data.Text.isInfixOf` TextShow.showt expandedPath)

spec :: Spec
spec = describe "ExpandedPath" $ do
    it "Eq ExpandedPath" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateExpandedPath)
    it "Ord ExpandedPath" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd generateExpandedPath)
    it "ExpandedPathRoundTrip" $
        HaskellWorks.Hspec.Hedgehog.require propertyExpandedPathRoundTrip
    it "ExpandedPathEndsWithSlashOnlyIfDirectory" $
        HaskellWorks.Hspec.Hedgehog.require propertyExpandedPathEndsWithSlashOnlyIfDirectory
    it "ExpandedPathShowMustNotContainDoubleSlash" $
        HaskellWorks.Hspec.Hedgehog.require propertyExpandedPathShowMustNotContainDoubleSlash
    it "TextShow /" $
        TextShow.showt rootAbsDir `shouldBe` "/"
    it "TextShow /home/hilco/" $
        TextShow.showt homeAbsDir `shouldBe` "/home/hilco/"
    it "TextShow src/main/haskell/" $
        TextShow.showt srcRelDir `shouldBe` "src/main/haskell/"
    it "TextShow src/main/haskell/Hilcode/Nullable.hs" $
        TextShow.showt nullableHsRelFile `shouldBe` "src/main/haskell/Hilcode/Nullable.hs"
