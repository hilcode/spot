module Hilcode.GlobSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec

import qualified Data.Vector
import qualified TextShow

import           Hilcode.ExpandedPath
import           Hilcode.FilePathType
import           Hilcode.Glob
import           Hilcode.PathComponentGlobs
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators
import           Hilcode.Util.Literals

invert :: FilePathType -> FilePathType
invert Absolute = Relative
invert Relative = Absolute

toFilePathType :: ExpandedPath -> FilePathType
toFilePathType (AbsoluteDirectory _) = Absolute
toFilePathType (AbsoluteFile      _) = Absolute
toFilePathType (RelativeDirectory _) = Relative
toFilePathType (RelativeFile      _) = Relative

propertyFilePathTypeMismatch :: Property
propertyFilePathTypeMismatch = property $ do
    expandedPath <- Hedgehog.forAll generateExpandedPath
    let glob :: Glob = Glob (invert (toFilePathType expandedPath)) pathComponentGlobs
    assertNot $ matchExpandedPathToGlob expandedPath glob

propertyManyDirectories :: Property
propertyManyDirectories = property $ do
    expandedPath <- Hedgehog.forAll generateExpandedPath
    let glob = Glob (toFilePathType expandedPath) (makePathComponentGlobs (Data.Vector.singleton manyDirectories))
    assert $ matchExpandedPathToGlob expandedPath glob

spec :: Spec
spec = describe "Glob" $ do
    it "Eq Glob" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateGlob)
    it "show globAbsolute" $
        TextShow.showt globAbsolute `shouldBe` "Glob{/?[a-z]*a/**/?[a-z]*a|*[0-9]aa??/?[a-z]*a|*[0-9]aa??|[0-9]*_?a?}"
    it "show globRelative" $
        TextShow.showt globRelative `shouldBe` "Glob{?[a-z]*a/**/?[a-z]*a|*[0-9]aa??/?[a-z]*a|*[0-9]aa??|[0-9]*_?a?}"
    it "FilePathTypeMismatch" $
        HaskellWorks.Hspec.Hedgehog.require propertyFilePathTypeMismatch
    it "ManyDirectories" $
        HaskellWorks.Hspec.Hedgehog.require propertyManyDirectories
    it "'xxxa/abc/def/aaaa/99999_9a' ~ glob" $
        matchExpandedPathToGlob (RelativeFile ["xxxa", "abc", "def", "aaaa", "99999_9a9"]) (Glob Relative pathComponentGlobs)
