module Hilcode.Util.Generators
( generateCharSet
, generateExpandedPath
, generateFilePathType
, generateGlob
, generateGlobPart
, generateGlobParts
, generateGlobPartsAndNullable
, generateGlobPartsAndText
, generateLowerUpper
, generatePathComponentGlob
, generatePathComponentGlobAndNullable
, generatePathComponentGlobs
, generatePathComponentGlobsAndNullable
, generateQueueAndList
, generateSimpleGlob
, generateSimpleGlobAndNullable
, generateSimpleGlobAndText
, generateThread
) where

import           Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range

import           Data.Text (Text)
import qualified Control.Applicative
import qualified Data.Char
import qualified Data.Foldable
import qualified Data.List
import qualified Data.Text
import qualified Data.Vector

import           Hilcode.CharSet
import           Hilcode.ExpandedPath
import           Hilcode.FilePathType
import           Hilcode.Glob
import           Hilcode.GlobPart
import           Hilcode.GlobParts
import           Hilcode.Internal.Queue
import           Hilcode.Internal.Thread
import           Hilcode.Nullable
import           Hilcode.PathComponentGlob
import           Hilcode.PathComponentGlobs
import           Hilcode.SimpleGlob
import           Hilcode.Util.Literals

generateThread :: forall part matcher . Gen part -> Gen matcher -> Gen (Thread part matcher)
generateThread generatePart generateMatcher = Hedgehog.Gen.frequency
    [ (1, pure Success)
    , (9, Thread <$> Hedgehog.Gen.list (Hedgehog.Range.constant 0 10) generatePart <*> Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) generateMatcher)
    ]

generateLowerUpper :: Gen (Char, Char)
generateLowerUpper = do
    lower' <- Hedgehog.Gen.latin1
    let lower = Data.Char.chr (Data.Char.ord lower' `div` 2)
    upper' <- Hedgehog.Gen.latin1
    if upper' < lower
        then Hedgehog.Gen.discard
        else pure (lower, upper')

generateCharSet :: Gen CharSet
generateCharSet = do
    n <- Hedgehog.Gen.int (Hedgehog.Range.constant 1 2)
    if  | n == 1 ->
            pure includeNone
        | n == 2 ->
            include <$> Hedgehog.Gen.alphaNum
        | otherwise -> do
            m <- Hedgehog.Gen.int (Hedgehog.Range.constant 1 3)
            if  | m == 1 ->
                    pure (includeRange ('0', '9'))
                | m == 2 ->
                    pure (includeRange ('a', 'z'))
                | otherwise ->
                    pure (includeRange ('A', 'Z'))

generateFilePartChar :: Gen Char
generateFilePartChar = Hedgehog.Gen.element (['a'..'z'] <> [' '..'.'] <> ['0'..'`'] <> ['{'..'\255']) -- ' '..'\255' minus '/'

generateFilePart :: Gen Text
generateFilePart = Data.Text.pack <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) generateFilePartChar

generateFileParts :: Gen [Text]
generateFileParts = Hedgehog.Gen.list (Hedgehog.Range.constant 1 5) generateFilePart

generateExpandedPath :: Gen ExpandedPath
generateExpandedPath = Hedgehog.Gen.frequency
    [ (1, Hedgehog.Gen.constant (AbsoluteDirectory []))
    , (9, Hedgehog.Gen.element [RelativeDirectory, AbsoluteDirectory, RelativeFile, AbsoluteFile] <*> generateFileParts)
    ]

generateFilePathType :: Gen FilePathType
generateFilePathType =
    Hedgehog.Gen.element [Relative, Absolute]

appendWithOr :: Nullable a => ([a], Bool) -> a -> ([a], Bool)
appendWithOr = append (||)

appendWithAnd :: Nullable a => ([a], Bool) -> a -> ([a], Bool)
appendWithAnd = append (&&)

append :: Nullable a => (Bool -> Bool -> Bool) -> ([a], Bool) -> a -> ([a], Bool)
append boolOp (as, asNullable) a = (as <> [a], asNullable `boolOp` nullable a)

generateGlobPart :: Gen GlobPart
generateGlobPart = Hedgehog.Gen.choice [pure Many, pure Any, Range <$> generateCharSet, Single <$> Hedgehog.Gen.alphaNum]

generateGlobPartsAndNullable :: Gen (GlobParts, Bool)
generateGlobPartsAndNullable = do
    (globParts, globPartsNullable) <- Data.Foldable.foldl' appendWithAnd ([], True) <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) generateGlobPart
    pure (makeGlobParts (Data.Vector.fromList globParts), globPartsNullable)

bimap :: (a -> c, b -> d) -> (a, b) -> (c, d)
bimap (f, g) (a, b) = (f a, g b)

apply :: Applicative applicative => (a -> c, b -> d) -> applicative (a, b) -> applicative (c, d)
apply f = Control.Applicative.liftA2 bimap (pure f)

generateGlobPartAndText :: Gen (GlobPart, String)
generateGlobPartAndText = Hedgehog.Gen.choice
    [ (Many, )                        <$> Hedgehog.Gen.list (Hedgehog.Range.constant 0 5) generateValidFilePartChar
    , (\ch -> (Any, [ch]))            <$> generateValidFilePartChar
    , (\ch -> (Range alphabet, [ch])) <$> Hedgehog.Gen.alpha
    , (\ch -> (Single ch, [ch]))      <$> generateValidFilePartChar
    ]

generateGlobPartsAndText :: Gen (GlobParts, Text)
generateGlobPartsAndText = (makeGlobParts . Data.Vector.fromList, Data.Text.pack . Data.List.concat) `apply` Data.List.unzip <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 5) generateGlobPartAndText

generateGlobParts :: Gen GlobParts
generateGlobParts = makeGlobParts . Data.Vector.fromList <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) generateGlobPart

generateSimpleGlob :: Gen SimpleGlob
generateSimpleGlob = makeSimpleGlob . Data.Vector.fromList <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 5) generateGlobParts

generateSimpleGlobAndText :: Gen (SimpleGlob, Text)
generateSimpleGlobAndText = do
    list <- Hedgehog.Gen.list (Hedgehog.Range.constant 1 5) generateGlobPartsAndText
    item <- snd <$> Hedgehog.Gen.element list
    pure (makeSimpleGlob (Data.Vector.fromList (fst <$> list)), item)

generateValidFilePartChar :: Gen Char
generateValidFilePartChar = Hedgehog.Gen.element ([' '..'.'] <> ['0'..'\255'])

generateSimpleGlobAndNullable :: Gen (SimpleGlob, Bool)
generateSimpleGlobAndNullable = do
    (globParts, simpleGlobNullable) <- Data.Foldable.foldl' appendWithOr ([], False) <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) generateGlobParts
    pure (makeSimpleGlob (Data.Vector.fromList globParts), Data.List.null globParts || simpleGlobNullable)

generatePathComponentGlob :: Gen PathComponentGlob
generatePathComponentGlob = Hedgehog.Gen.frequency
    [ (1, pure ManyDirectories)
    , (9, PathComponentGlob <$> generateSimpleGlob)
    ]

generatePathComponentGlobAndNullable :: Gen (PathComponentGlob, Bool)
generatePathComponentGlobAndNullable = do
    pathComponentGlob <- generatePathComponentGlob
    case pathComponentGlob of
        ManyDirectories -> pure (pathComponentGlob, True)
        PathComponentGlob simpleGlob -> pure (pathComponentGlob, nullable simpleGlob)

generatePathComponentGlobs :: Gen PathComponentGlobs
generatePathComponentGlobs = makePathComponentGlobs . Data.Vector.fromList <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 5) generatePathComponentGlob

generatePathComponentGlobsAndNullable :: Gen (PathComponentGlobs, Bool)
generatePathComponentGlobsAndNullable = do
    (pathComponentGlobs, pathComponentGlobsNullable) <- Data.Foldable.foldl' appendWithAnd ([], True) <$> Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) generatePathComponentGlob
    pure (makePathComponentGlobs (Data.Vector.fromList pathComponentGlobs), pathComponentGlobsNullable)

generateGlob :: Gen Glob
generateGlob = Glob <$> generateFilePathType <*> generatePathComponentGlobs

generateQueueAndList :: Gen (Queue Int, [Int])
generateQueueAndList = Hedgehog.Gen.frequency
    [ (1, pure (empty, []))
    , (2, headsOnly)
    , (2, tailsOnly)
    , (5, headsAndTails)
    ]
  where
    headsOnly = do
        heads <- Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) (Hedgehog.Gen.int (Hedgehog.Range.constant 1 1000))
        pure (Queue heads [], heads)
    tailsOnly = do
        tails <- Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) (Hedgehog.Gen.int (Hedgehog.Range.constant 1 1000))
        pure (Queue [] (reverse tails), tails)
    headsAndTails = do
        heads <- Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) (Hedgehog.Gen.int (Hedgehog.Range.constant 1 1000))
        tails <- Hedgehog.Gen.list (Hedgehog.Range.constant 1 10) (Hedgehog.Gen.int (Hedgehog.Range.constant 1 1000))
        pure (Queue heads (reverse tails), heads <> tails)
