module Hilcode.ThreadSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec
import qualified Hedgehog.Gen
import qualified Hedgehog.Range

import           Data.Vector (Vector)

import qualified Data.Foldable
import qualified Data.Set
import qualified Data.Vector

import           Hilcode.Internal.Thread
import           Hilcode.Nullable
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators

data Matcher = T | OT | F | OF | A | M deriving (Eq, Ord, Show)

instance Nullable Matcher where
    nullable T  = False
    nullable OT = True
    nullable F  = False
    nullable OF = True
    nullable A  = False
    nullable M  = True

instance HasThread Bool Matcher where
    partMatcher :: Bool -> Matcher -> Bool
    partMatcher True  T  = True
    partMatcher True  OT = True
    partMatcher True  F  = False
    partMatcher True  OF = False
    partMatcher True  A  = True
    partMatcher True  M  = True
    partMatcher False T  = False
    partMatcher False OT = False
    partMatcher False F  = True
    partMatcher False OF = True
    partMatcher False A  = True
    partMatcher False M  = True

generateMatcherAndParts :: Gen (Matcher, [Bool])
generateMatcherAndParts = Hedgehog.Gen.choice
    [ pure (T, [True])
    , Hedgehog.Gen.choice [pure (OT, []), pure (OT, [True])]
    , pure (F, [False])
    , Hedgehog.Gen.choice [pure (OF, []), pure (OF, [False])]
    , (A, ) <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 1) Hedgehog.Gen.bool
    , (M, ) <$> Hedgehog.Gen.list (Hedgehog.Range.constant 0 4) Hedgehog.Gen.bool
    ]

toVector :: (Matcher, [Bool]) -> (Vector Matcher, [Bool])
toVector (matcher, bools) = (Data.Vector.singleton matcher, bools)

append :: (Vector Matcher, [Bool]) -> (Vector Matcher, [Bool]) -> (Vector Matcher, [Bool])
append (lftMatchers, lftBools) (rgtMatchers, rgtBools) = (lftMatchers <> rgtMatchers, lftBools <> rgtBools)

generateMatchersAndParts :: Gen (Vector Matcher, [Bool])
generateMatchersAndParts = do
    list <- Hedgehog.Gen.list (Hedgehog.Range.constant 0 4) generateMatcherAndParts
    pure $ Data.Foldable.foldl' append (Data.Vector.empty, []) (toVector <$> list)

propertyThread :: Property
propertyThread = property $ do
    (matchers, parts) <- Hedgehog.forAll generateMatchersAndParts
    annotate $ show parts <> " --> " <> show matchers
    assert $ match parts matchers

noParts :: [Bool]
noParts = []

noMatchers :: Vector Matcher
noMatchers = Data.Vector.empty

generatePart :: Gen Bool
generatePart = Hedgehog.Gen.bool

generateMatcher :: Gen Matcher
generateMatcher = Hedgehog.Gen.frequency
    [ (1, pure T)
    , (1, pure OT)
    , (1, pure F)
    , (1, pure OF)
    , (1, pure A)
    , (1, pure M)
    ]

threadSuccess :: Thread Bool Matcher
threadSuccess = Success

spec :: Spec
spec = describe "Thread" $ do
    it "Eq Thread" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq (generateThread generatePart generateMatcher))
    it "Ord Thread" $
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckOrd (generateThread generatePart generateMatcher))
    it "Thread" $
        HaskellWorks.Hspec.Hedgehog.require propertyThread
    it "stepThread Success             --> {Success}" $
        stepThread threadSuccess                                  `shouldBe` Data.Set.singleton threadSuccess
    it "[]                    ~ [A]    --> False" $
        match noParts               (Data.Vector.singleton A)     `shouldBe` False
    it "[True]                ~ [M, T] --> True" $
        match [True]                (Data.Vector.fromList [M, T]) `shouldBe` True
    it "[False, False]        ~ [M]    --> True" $
        match [False, False]        (Data.Vector.fromList [M])    `shouldBe` True
    it "[True]                ~ []     --> False" $
        match [True]                noMatchers                    `shouldBe` False
    it "[False]               ~ []     --> False" $
        match [False]               noMatchers                    `shouldBe` False
    it "[True, True]          ~ []     --> False" $
        match [True, True]          noMatchers                    `shouldBe` False
    it "[True, False]         ~ []     --> False" $
        match [True, False]         noMatchers                    `shouldBe` False
    it "[False, True]         ~ []     --> False" $
        match [False, True]         noMatchers                    `shouldBe` False
    it "[False, False]        ~ []     --> False" $
        match [False, False]        noMatchers                    `shouldBe` False
    it "[True]                ~ [F]    --> False" $
        match [True]                (Data.Vector.singleton F)     `shouldBe` False
    it "[False]               ~ [T]    --> False" $
        match [False]               (Data.Vector.singleton T)     `shouldBe` False
    it "[True, True]          ~ [T]    --> False" $
        match [True, True]          (Data.Vector.singleton T)     `shouldBe` False
    it "[True, False]         ~ [T]    --> False" $
        match [True, False]         (Data.Vector.singleton T)     `shouldBe` False
    it "[False, True]         ~ [T]    --> False" $
        match [False, True]         (Data.Vector.singleton T)     `shouldBe` False
    it "[False, False]        ~ [T]    --> False" $
        match [False, False]        (Data.Vector.singleton T)     `shouldBe` False
    it "[True, True]          ~ [F]    --> False" $
        match [True, True]          (Data.Vector.singleton F)     `shouldBe` False
    it "[True, False]         ~ [F]    --> False" $
        match [True, False]         (Data.Vector.singleton F)     `shouldBe` False
    it "[False, True]         ~ [F]    --> False" $
        match [False, True]         (Data.Vector.singleton F)     `shouldBe` False
    it "[False, False]        ~ [F]    --> False" $
        match [False, False]        (Data.Vector.singleton F)     `shouldBe` False
    it "[True, True, False]   ~ [M, T] --> False" $
        match [True, True, False]   (Data.Vector.fromList [M, T]) `shouldBe` False
    it "[True, False, False]  ~ [M, T] --> False" $
        match [True, False, False]  (Data.Vector.fromList [M, T]) `shouldBe` False
    it "[False, True, False]  ~ [M, T] --> False" $
        match [False, True, False]  (Data.Vector.fromList [M, T]) `shouldBe` False
    it "[False, False, False] ~ [M, T] --> False" $
        match [False, False, False] (Data.Vector.fromList [M, T]) `shouldBe` False
