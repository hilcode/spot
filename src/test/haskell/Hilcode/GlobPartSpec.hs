module Hilcode.GlobPartSpec (spec) where

import Prelude hiding (any)

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec

import qualified TextShow

import           Hilcode.CharSet
import           Hilcode.GlobPart
import           Hilcode.Nullable
import           Hilcode.Thread
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators
import           Hilcode.Util.Literals

isMany :: GlobPart -> Bool
isMany Many = True
isMany _    = False

propertyNullable :: Property
propertyNullable = property $ do
    globPart <- Hedgehog.forAll generateGlobPart
    nullable globPart === isMany globPart

spec :: Spec
spec = describe "GlobPart" $ do
    it "Eq GlobPart" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateGlobPart)
    it "Ord GlobPart" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd generateGlobPart)
    it "Nullable GlobPart" $
        HaskellWorks.Hspec.Hedgehog.require propertyNullable
    it "Many                    --> '*'" $
        TextShow.showt many                                                         `shouldBe` "*"
    it "Any                     --> '?'" $
        TextShow.showt any                                                          `shouldBe` "?"
    it "Range [a]               --> '[a]'" $
        TextShow.showt (Range (include 'a'))                                        `shouldBe` "[a]"
    it "Range [0-9a-z]          --> '[0-9a-z]'" $
        TextShow.showt (Range (includeRange ('a', 'z') <> includeRange ('0', '9'))) `shouldBe` "[0-9a-z]"
    it "Single 'a'              --> 'a'" $
        TextShow.showt a                                                            `shouldBe` "a"
    it "nullable Many           --> True" $
        nullable many                                                               `shouldBe` True
    it "nullable Any            --> False" $
        nullable any                                                                `shouldBe` False
    it "nullable Range [a]      --> False" $
        nullable (Range (include 'a'))                                              `shouldBe` False
    it "nullable Range [0-9a-z] --> False" $
        nullable (Range (includeRange ('a', 'z') <> includeRange ('0', '9')))       `shouldBe` False
    it "nullable Single 'a'     --> False" $
        nullable a                                                                  `shouldBe` False
    it "Many ~ 'a'              --> True" $
        partMatcher 'a' many                                                        `shouldBe` True
    it "Any ~ 'a'               --> True" $
        partMatcher 'a' any                                                         `shouldBe` True
    it "Range [a-z] ~ 'a'       --> True" $
        partMatcher 'a' (Range (includeRange ('a', 'z')))                           `shouldBe` True
    it "Range [a-z] ~ '0'       --> False" $
        partMatcher '0' (Range (includeRange ('a', 'z')))                           `shouldBe` False
    it "Single 'a' ~ 'a'        --> True" $
        partMatcher 'a' a                                                           `shouldBe` True
    it "Single 'a' ~ '0'        --> False" $
        partMatcher '0' a                                                           `shouldBe` False
