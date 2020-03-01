module Hilcode.Util.Literals where

import Prelude hiding (any)

import qualified Data.Vector

import           Hilcode.CharSet
import           Hilcode.ExpandedPath
import           Hilcode.FilePathType
import           Hilcode.Glob
import           Hilcode.GlobPart
import           Hilcode.GlobParts
import           Hilcode.PathComponentGlob
import           Hilcode.PathComponentGlobs
import           Hilcode.SimpleGlob

rootAbsDir :: ExpandedPath
rootAbsDir = AbsoluteDirectory []

homeAbsDir :: ExpandedPath
homeAbsDir = AbsoluteDirectory ["home", "hilco"]

srcRelDir :: ExpandedPath
srcRelDir = RelativeDirectory ["src", "main", "haskell"]

nullableHsRelFile :: ExpandedPath
nullableHsRelFile = RelativeFile ["src", "main", "haskell", "Hilcode", "Nullable.hs"]

charSetDigits :: CharSet
charSetDigits = includeRange ('0', '9')

charSetLettersLowercase :: CharSet
charSetLettersLowercase = includeRange ('a', 'z')

charSetLettersUppercase :: CharSet
charSetLettersUppercase = includeRange ('A', 'Z')

alphabet :: CharSet -- [A-Z][a-z]
alphabet = charSetLettersLowercase <> charSetLettersUppercase

many :: GlobPart -- *
many = Many

any :: GlobPart -- ?
any = Any

letters :: GlobPart -- [a-z]
letters = Range charSetLettersLowercase

digits :: GlobPart -- [0-9]
digits = Range charSetDigits

a :: GlobPart -- a
a = Single 'a'

underscore :: GlobPart -- _
underscore = Single '_'

globPartsA :: GlobParts -- ?[a-z]*a
globPartsA = makeGlobParts $ Data.Vector.fromList [any, letters, many, a]

globPartsB :: GlobParts -- *[0-9]aa??
globPartsB = makeGlobParts $ Data.Vector.fromList [many, digits, a, a, any, any]

globPartsC :: GlobParts -- [0-9]*_?a?
globPartsC = makeGlobParts $ Data.Vector.fromList [digits, many, underscore, any, a, any]

simpleGlobOne :: SimpleGlob -- ?[a-z]*a
simpleGlobOne = makeSimpleGlob $ Data.Vector.singleton globPartsA

simpleGlobTwo :: SimpleGlob -- ?[a-z]*a|*[0-9]aa??
simpleGlobTwo = makeSimpleGlob $ Data.Vector.fromList [globPartsA, globPartsB]

simpleGlobThree :: SimpleGlob -- ?[a-z]*a|*[0-9]aa??|[0-9]*_?a?
simpleGlobThree = makeSimpleGlob $ Data.Vector.fromList [globPartsA, globPartsB, globPartsC]

pathComponentGlobOne :: PathComponentGlob -- ?[a-z]*a
pathComponentGlobOne = PathComponentGlob simpleGlobOne

pathComponentGlobTwo :: PathComponentGlob -- ?[a-z]*a|*[0-9]aa??
pathComponentGlobTwo = PathComponentGlob simpleGlobTwo

pathComponentGlobThree :: PathComponentGlob -- ?[a-z]*a|*[0-9]aa??|[0-9]*_?a?
pathComponentGlobThree = PathComponentGlob simpleGlobThree

manyDirectories :: PathComponentGlob -- **
manyDirectories = ManyDirectories

pathComponentGlobs :: PathComponentGlobs -- ?[a-z]*a/**/?[a-z]*a|*[0-9]aa??/?[a-z]*a|*[0-9]aa??|[0-9]*_?a?
pathComponentGlobs = makePathComponentGlobs $ Data.Vector.fromList [pathComponentGlobOne, manyDirectories, pathComponentGlobTwo, pathComponentGlobThree]

globAbsolute :: Glob
globAbsolute = Glob Absolute pathComponentGlobs

globRelative :: Glob
globRelative = Glob Relative pathComponentGlobs
