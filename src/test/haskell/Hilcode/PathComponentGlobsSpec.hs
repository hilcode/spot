module Hilcode.PathComponentGlobsSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec

import qualified Control.Exception
import qualified Data.Vector
import qualified TextShow

import           Hilcode.Nullable
import           Hilcode.PathComponentGlobs
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators
import           Hilcode.Util.Literals

propertyNullable :: Property
propertyNullable = property $ do
    (pathComponentGlobs, pathComponentGlobsNullable) <- Hedgehog.forAll generatePathComponentGlobsAndNullable
    assert $ nullable pathComponentGlobs == pathComponentGlobsNullable

spec :: Spec
spec = describe "PathComponentGlobs" $ do
    it "Eq GlobParts" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generatePathComponentGlobs)
    it "Nullable GlobParts" $
        HaskellWorks.Hspec.Hedgehog.require propertyNullable
    it "Show pathComponentGlobs" $
        TextShow.showt pathComponentGlobs `shouldBe` "?[a-z]*a/**/?[a-z]*a|*[0-9]aa??/?[a-z]*a|*[0-9]aa??|[0-9]*_?a?"
    it "'xxxa/abc/def/aaaa/99999_9a' ~ pathComponentGlobs" $
        matchTextsToPathComponentGlobs ["xxxa", "abc", "def", "aaaa", "99999_9a9"] pathComponentGlobs
    it "A PathComponentGlobs must have at least 1 PathComponentGlob" $
        Control.Exception.evaluate (makePathComponentGlobs Data.Vector.empty) `shouldThrow` errorCall "A 'PathComponentGlobs' _must_ have at least 1 'PathComponentGlob'."
