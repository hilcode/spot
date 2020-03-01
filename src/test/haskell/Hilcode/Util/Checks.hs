module Hilcode.Util.Checks where

import Hedgehog

assertNot :: Bool -> PropertyT IO ()
assertNot predicate = assert (not predicate)

endsWith :: Eq a => [a] -> a -> Bool
endsWith []       _        = False
endsWith (_:rest) expected = endsWith rest expected

propertyCheckEq :: (Show a, Eq a) => Gen a -> Property
propertyCheckEq generator = property $ do
    (lft, rgt) <- generateLftAndRgt generator
    assert $ lft == lft
    assert $ rgt == rgt
    assert $ (lft == rgt) == (rgt == lft)
    footnote $ "     lft == rgt      --> " <> show (lft == rgt)
    footnote $ "show lft == show rgt --> " <> show (show lft == show rgt)
    assert $ (lft == rgt) == (show lft == show rgt)
    assert $ (rgt /= lft) == (lft /= rgt)
    assert $ (lft /= rgt) == (show lft /= show rgt)

propertyCheckOrd :: (Show a, Ord a) => Gen a -> Property
propertyCheckOrd generator = property $ do
    (lft, rgt) <- generateLftAndRgt generator
    assertNot $ lft < lft
    assertNot $ lft > lft
    assert $ rgt <= rgt
    assert $ rgt >= rgt
    assert $ lft `compare` rgt /= rgt `compare` lft || lft == rgt
    assert $ lft `min` rgt < rgt `max` lft || lft == rgt
    assert $ lft `max` rgt > rgt `min` lft || lft == rgt
    assert $ lft `min` rgt == rgt `min` lft
    assert $ lft `max` rgt == rgt `max` lft
    assert $ lft `min` rgt /= rgt `max` lft || lft == rgt
    assert $ lft `max` rgt /= rgt `min` lft || rgt == lft
    assert $ (lft > rgt) == (rgt < lft)
    assert $ (lft < rgt) == (rgt > lft)
    assert $ (lft >= rgt) == (rgt <= lft)
    assert $ (lft <= rgt) == (rgt >= lft)

generateLftAndRgt :: Show a => Gen a -> PropertyT IO (a, a)
generateLftAndRgt generator = do
    lft <- Hedgehog.forAll generator
    footnote $ "lft = '" <> show lft <> "'"
    rgt <- Hedgehog.forAll generator
    footnote $ "rgt = '" <> show rgt <> "'"
    pure (lft, rgt)
