module Hilcode.QueueSpec (spec) where

import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import           Test.Hspec

-- import qualified Data.Maybe
-- import qualified Data.Vector
-- import qualified TextShow

import           Hilcode.Internal.Queue
import           Hilcode.Util.Checks
import           Hilcode.Util.Generators
-- import           Hilcode.Util.Literals

propertyQueueRoundtrip :: Property
propertyQueueRoundtrip = property $ do
    (queue, list) <- Hedgehog.forAll generateQueueAndList
    assert $ queue == fromList list
    assert $ list == toList queue

sentinel :: Int
sentinel = 1001

propertyPush :: Property
propertyPush = property $ do
    (queue, list) <- Hedgehog.forAll generateQueueAndList
    assert $ toList (push 1001 queue) == list <> [sentinel]

propertyPop :: Property
propertyPop = property $ do
    (queue, list) <- Hedgehog.forAll generateQueueAndList
    case pop queue of
        Nothing             -> assert $ isEmpty queue
        Just (head, queue') -> assert $ head : toList queue' == list

propertySemigroup :: Property
propertySemigroup = property $ do
    (queue1, list1) <- Hedgehog.forAll generateQueueAndList
    (queue2, list2) <- Hedgehog.forAll generateQueueAndList
    assert $ toList (queue1 <> queue2) == list1 <> list2

propertyFoldable :: Property
propertyFoldable = property $ do
    (queue, list) <- Hedgehog.forAll generateQueueAndList
    assert $ sum queue == sum list

propertyFunctor :: Property
propertyFunctor = property $ do
    (queue, list) <- Hedgehog.forAll generateQueueAndList
    assert $ toList (fmap (2 *) queue) == fmap (2 *) list

propertyApplicative :: Property
propertyApplicative = property $ do
    (queueA, listA) <- Hedgehog.forAll generateQueueAndList
    let (queueF, listF) = (fmap (const ((2::Int) *)) queueA, fmap (const ((2::Int) *)) listA)
    assert $ toList (queueF <*> queueA) == (listF <*> listA)

function :: (Semigroup (monad a), Monad monad) => a -> monad a
function a = pure a <> pure a

propertyMonad :: Property
propertyMonad = property $ do
    (queue, list) <- Hedgehog.forAll generateQueueAndList
    assert $ toList (queue >>= function) == (list >>= function)

spec :: Spec
spec = describe "Queue" $ do
    it "Queue" $
        HaskellWorks.Hspec.Hedgehog.require propertyQueueRoundtrip
    it "Eq Queue" $ do
        HaskellWorks.Hspec.Hedgehog.require (propertyCheckEq generateQueueAndList)
        Queue [] [5::Int, 4, 3, 2, 1] `shouldBe` Queue [1, 2] [5, 4, 3]
        Queue [1, 2] [5, 4, 3] `shouldBe` Queue [] [5::Int, 4, 3, 2, 1]
    it "Functor Queue" $
        HaskellWorks.Hspec.Hedgehog.require propertyFunctor
    it "Applicative Queue" $
        HaskellWorks.Hspec.Hedgehog.require propertyApplicative
    it "Monad Queue" $
        HaskellWorks.Hspec.Hedgehog.require propertyMonad
    it "Foldable Queue" $
        HaskellWorks.Hspec.Hedgehog.require propertyFoldable
    it "Semigroup Queue" $
        HaskellWorks.Hspec.Hedgehog.require propertySemigroup
    it "Queue::push" $
        HaskellWorks.Hspec.Hedgehog.require propertyPush
    it "Queue::pop" $
        HaskellWorks.Hspec.Hedgehog.require propertyPop
    it "Queue::singleton" $ do
        toList (singleton sentinel) `shouldBe` [sentinel]
        singleton sentinel `shouldBe` pure sentinel
        singleton sentinel `shouldBe` return sentinel
        isEmpty (singleton sentinel) `shouldBe` False
