module Hilcode.CharSet
( CharSet
, contains
, exclude
, excludeNone
, excludeRange
, exclusions
, fromExclusiveToInclusive
, include
, includeNone
, includeRange
, inclusions
, size
) where

import           Data.IntSet (IntSet, (\\))
import           TextShow (TextShow)
import qualified Data.Char
import qualified Data.IntSet
import qualified TextShow
import qualified TextShow.Data.Char

data CharSetType = Inclusive | Exclusive
    deriving (Eq, Ord, Show)

data CharSet = CharSet CharSetType IntSet
    deriving (Eq, Ord, Show)

instance TextShow CharSet where
    showb (CharSet charSetType intSet) = (if charSetType == Inclusive then "[" else "[^") <> go "" Nothing (Data.Char.chr <$> Data.IntSet.toList intSet) <> "]"
      where
        go :: TextShow.Builder -> Maybe (Char, Char) -> String -> TextShow.Builder
        go output (Just (lower, upper)) []   = output <> chars lower upper
        go output Nothing               []   = output
        go output (Just (lower, upper)) [ch]
            | succ upper == ch               = output <> chars lower ch
            | otherwise                      = output <> chars lower upper <> char ch
        go output Nothing               [ch] = output <> char ch
        go output (Just (lower, upper)) rest@(ch:chs)
            | succ upper == ch               = go output (Just (lower, ch)) chs
            | otherwise                      = go (output <> chars lower upper) Nothing rest
        go output Nothing [ch1, ch2]         = output <> char ch1 <> char ch2
        go output Nothing (ch1:rest@(_:ch3:chs))
            | (succ . succ) ch1 == ch3       = go output (Just (ch1, ch3)) chs
            | otherwise                      = go (output <> char ch1) Nothing rest
        char :: Char -> TextShow.Builder
        char = TextShow.Data.Char.showbLitChar
        chars :: Char -> Char -> TextShow.Builder
        chars lower upper = char lower <> char '-' <> char upper

instance Semigroup CharSet where
    CharSet Inclusive lft <> CharSet Inclusive rgt = CharSet Inclusive (lft <> rgt)
    CharSet Exclusive lft <> CharSet Exclusive rgt = CharSet Exclusive (lft <> rgt)
    lft@(CharSet Inclusive _) <> rgt@(CharSet Exclusive _) = lft <> fromExclusiveToInclusive rgt
    lft@(CharSet Exclusive _) <> rgt@(CharSet Inclusive _) = fromExclusiveToInclusive lft <> rgt

everything :: IntSet
everything = Data.IntSet.fromDistinctAscList [minBound..maxBound]

fromExclusiveToInclusive :: CharSet -> CharSet
fromExclusiveToInclusive inclusiveCharSet@(CharSet Inclusive _) = inclusiveCharSet
fromExclusiveToInclusive (CharSet Exclusive intSet) = CharSet Inclusive (everything \\ intSet)

instance Monoid CharSet where
    mempty = CharSet Inclusive Data.IntSet.empty

includeNone :: CharSet
includeNone = empty Inclusive

excludeNone :: CharSet
excludeNone = empty Exclusive

empty :: CharSetType -> CharSet
empty charSetType = CharSet charSetType Data.IntSet.empty

include :: Char -> CharSet
include = singleton Inclusive

exclude :: Char -> CharSet
exclude = singleton Exclusive

singleton :: CharSetType -> Char -> CharSet
singleton charSetType ch = CharSet charSetType (Data.IntSet.singleton (Data.Char.ord ch))

includeRange :: (Char, Char) -> CharSet
includeRange = range Inclusive

excludeRange :: (Char, Char) -> CharSet
excludeRange = range Exclusive

range :: CharSetType -> (Char, Char) -> CharSet
range charSetType (lower, upper) = CharSet charSetType (Data.IntSet.fromList [Data.Char.ord lower..Data.Char.ord upper])

contains :: CharSet -> Char -> Bool
contains (CharSet Inclusive intSet) ch = Data.IntSet.member (Data.Char.ord ch) intSet
contains (CharSet Exclusive intSet) ch = Data.IntSet.notMember (Data.Char.ord ch) intSet

size :: CharSet -> Int
size (CharSet _ intSet) = Data.IntSet.size intSet

{-# ANN inclusions ("HLint: ignore Use String"::String) #-}
inclusions :: [Char] -> CharSet
inclusions = fromList Inclusive

{-# ANN exclusions ("HLint: ignore Use String"::String) #-}
exclusions :: [Char] -> CharSet
exclusions = fromList Exclusive

{-# ANN fromList ("HLint: ignore Use String"::String) #-}
fromList :: CharSetType -> [Char] -> CharSet
fromList charSetType chs = (CharSet charSetType . Data.IntSet.fromList) (Data.Char.ord <$> chs)
