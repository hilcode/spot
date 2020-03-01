module Hilcode.Glob
( Glob(..)
, matchExpandedPathToGlob
, parseGlob
) where

import Prelude hiding (any, error)

import           Hilcode.ExpandedPath
import           Hilcode.FilePathType
import           Hilcode.GlobPart
import           Hilcode.GlobParts
import           Hilcode.Misc
import           Hilcode.Parser
import           Hilcode.PathComponentGlob
import           Hilcode.PathComponentGlobs
import           Hilcode.Result
import           Hilcode.SimpleGlob

import           Hilcode.CharSet (CharSet)
import           Control.Applicative ((<|>))
import           Data.Text (Text)
import           TextShow (TextShow)
import qualified Hilcode.CharSet
import qualified Control.Applicative
import qualified Data.Foldable
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Vector
import qualified TextShow
import qualified TextShow.Data.Char

data Glob
    = Glob FilePathType PathComponentGlobs
    deriving (Eq, Show)

instance TextShow Glob where
    showb (Glob Relative pathComponentGlobs) = TextShow.fromText "Glob{" <> TextShow.showb pathComponentGlobs <> TextShow.Data.Char.showbLitChar '}'
    showb (Glob Absolute pathComponentGlobs) = TextShow.fromText "Glob{/" <> TextShow.showb pathComponentGlobs <> TextShow.Data.Char.showbLitChar '}'

matchExpandedPathToGlob :: ExpandedPath -> Glob -> Bool
matchExpandedPathToGlob (AbsoluteDirectory _)              (Glob Relative _)                  = False
matchExpandedPathToGlob (AbsoluteFile      _)              (Glob Relative _)                  = False
matchExpandedPathToGlob (RelativeDirectory _)              (Glob Absolute _)                  = False
matchExpandedPathToGlob (RelativeFile      _)              (Glob Absolute _)                  = False
matchExpandedPathToGlob (AbsoluteDirectory pathComponents) (Glob Absolute pathComponentGlobs) = matchTextsToPathComponentGlobs pathComponents pathComponentGlobs
matchExpandedPathToGlob (AbsoluteFile      pathComponents) (Glob Absolute pathComponentGlobs) = matchTextsToPathComponentGlobs pathComponents pathComponentGlobs
matchExpandedPathToGlob (RelativeDirectory pathComponents) (Glob Relative pathComponentGlobs) = matchTextsToPathComponentGlobs pathComponents pathComponentGlobs
matchExpandedPathToGlob (RelativeFile      pathComponents) (Glob Relative pathComponentGlobs) = matchTextsToPathComponentGlobs pathComponents pathComponentGlobs

parseMany :: Parser GlobPart
parseMany = do
    _ <- char '*'
    pure Many

parseAny :: Parser GlobPart
parseAny = do
    _ <- char '?'
    pure Any

parseRange :: Parser GlobPart
parseRange = do
    _ <- char '['
    maybeMinus <- optional (char '-')
    let minus = Data.Maybe.isJust maybeMinus
    sets :: [CharSet] <- Control.Applicative.many (parseSimpleRange <|> parseEscapedRightBracket <|> parseSingle)
    _ <- char ']'
    let allSets = if minus then Hilcode.CharSet.include '-' : sets else sets
    if Data.List.null allSets
        then error "Empty range."
        else pure (Range (Data.Foldable.foldl' (<>) Hilcode.CharSet.includeNone allSets))
  where
    parseSimpleRange = do
        lower <- any
        _ <- char '-'
        upper <- any
        pure (Hilcode.CharSet.includeRange (lower, upper))
    parseEscapedRightBracket = do
        _ <- char '\\'
        _ <- char ']'
        pure (Hilcode.CharSet.include ']')
    parseSingle = Hilcode.CharSet.include <$> anyBut (Hilcode.CharSet.include ']')

parseChar :: Parser GlobPart
parseChar = parseCharMany <|> parseCharAny <|> parseCharLeftBracket <|> parseCharPipe <|> Single <$> anyBut (Hilcode.CharSet.inclusions ('/':['\0'..'\US']))
  where
    parseCharMany = do
        _ <- char '\\' >> char '*'
        pure (Single '*')
    parseCharAny = do
        _ <- char '\\' >> char '?'
        pure (Single '?')
    parseCharLeftBracket = do
        _ <- char '\\' >> char '['
        pure (Single '[')
    parseCharPipe = do
        _ <- char '\\' >> char '|'
        pure (Single '|')

parseGlobPart :: Parser GlobPart
parseGlobPart = parseMany <|> parseAny <|> parseRange <|> parseChar

parseGlobParts :: Parser GlobParts
parseGlobParts = do
    globParts <- Control.Applicative.some parseGlobPart
    let _a = removeDuplicates Many globParts
    pure (makeGlobParts (Data.Vector.fromList _a))

parseSimpleGlob :: Parser SimpleGlob
parseSimpleGlob = do
    firstGlobParts <- parseGlobParts
    nextGlobParts <- Control.Applicative.many (char '|' >> parseGlobParts)
    pure (makeSimpleGlob (Data.Vector.fromList (firstGlobParts:nextGlobParts)))

parsePathComponentGlob :: Parser PathComponentGlob
parsePathComponentGlob = parseManyDirectories <|> PathComponentGlob <$> parseSimpleGlob
  where
    parseManyDirectories = do
        _ <- text "**"
        slash <- lookAhead '/'
        if slash
            then pure ManyDirectories
            else error "Only '**' is valid here."

parsePathComponentGlobs :: Parser PathComponentGlobs
parsePathComponentGlobs = do
    firstPathComponentGlob <- parsePathComponentGlob
    nextPathComponentGlobs <- Control.Applicative.many (char '/' >> parsePathComponentGlob)
    pure (makePathComponentGlobs (Data.Vector.fromList (firstPathComponentGlob:nextPathComponentGlobs)))

globParser :: Parser Glob
globParser = do
    maybeSlash <- optional (char '/')
    let filePathType = if Data.Maybe.isJust maybeSlash then Absolute else Relative
    Glob filePathType <$> parsePathComponentGlobs

parseGlob :: Text -> Result Text Glob
parseGlob input = parse input globParser
