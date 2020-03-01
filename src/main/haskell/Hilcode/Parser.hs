module Hilcode.Parser
( Parser
, Eol(..)
, Hilcode.Parser.any
, Hilcode.Parser.error
, Hilcode.Parser.range
, anyBut
, char
, check
, eof
, eol
, lookAhead
, optional
, parse
, text
) where

import           GHC.Generics

import           Control.Applicative (Alternative)
import           Data.Text (Text)

import qualified Control.Applicative
import qualified Data.Bifunctor
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Text

import           Hilcode.CharSet
import           Hilcode.Misc
import           Hilcode.Result

newtype Parser a = Parser (String -> Maybe (Result Text (String, a)))

instance Functor Parser where
    fmap f (Parser parse) = Parser ((fmap . fmap . fmap . fmap) f parse)

instance Applicative Parser where
    pure a = Parser (\input -> Just (Success (input, a)))
    liftA2 f (Parser parseA) (Parser parseB) = Parser parse
      where
        parse input = case parseA input of
            Nothing                    -> Nothing
            Just (Failure failure)     -> Just (Failure failure)
            Just (Success (input', a)) -> (fmap . fmap) (Data.Bifunctor.second (f a)) (parseB input')

instance Monad Parser where
    return = pure
    Parser parseA >>= f = Parser parse
      where
        parse input = case parseA input of
            Nothing                    -> Nothing
            Just (Failure failure)     -> Just (Failure failure)
            Just (Success (input', a)) -> case f a of
                Parser parseB -> parseB input'

instance Semigroup a => Semigroup (Parser a) where
    Parser lft <> Parser rgt = Parser parse
      where
        parse input = case lft input of
            Nothing                       -> Nothing
            Just (Failure failure)        -> Just (Failure failure)
            Just (Success (input', lft')) -> case rgt input' of
                Nothing                        -> Nothing
                Just (Failure failure)         -> Just (Failure failure)
                Just (Success (input'', rgt')) -> Just (Success (input'', lft' <> rgt'))

instance Monoid a => Monoid (Parser a) where
    mempty = Parser (\input -> Just (Success (input, mempty)))

instance Alternative Parser where
    empty = Parser (const Nothing)
    Parser parseA <|> Parser parseB = Parser parse
      where
        parse input = case parseA input of
            Just result -> Just result
            Nothing     -> parseB input

parse :: forall a . Text -> Parser a -> Result Text a
parse input (Parser parseA) = case parseA (Data.Text.unpack input) of
    Nothing                -> Failure "No match."
    Just (Failure failure) -> Failure failure
    Just (Success (_, a))  -> Success a

check :: Text -> Parser Bool
check expected = Parser (\input -> Just (Success (input, Data.Maybe.isJust (Data.List.stripPrefix (Data.Text.unpack expected) input))))

lookAhead :: Char -> Parser Bool
lookAhead expected = Parser (\input -> Just (Success (input, expected `isFirstOf` input)))

error :: Text -> Parser a
error text = Parser (\_ -> Just (Failure text))

any :: Parser Char
any = Parser parse
  where
    parse :: String -> Maybe (Result Text (String, Char))
    parse []            = Nothing
    parse (actual:rest) = Just (Success (rest, actual))

anyBut :: CharSet -> Parser Char
anyBut exclusions = Parser parse
  where
    parse :: String -> Maybe (Result Text (String, Char))
    parse []            = Nothing
    parse (actual:rest) = if exclusions `contains` actual
      then Nothing
      else Just (Success (rest, actual))

optional :: Parser a -> Parser (Maybe a)
optional (Parser parseA) = Parser parse
  where
    parse input = case parseA input of
        Nothing                        -> Just (Success (input, Nothing))
        Just (Failure _)               -> Just (Success (input, Nothing))
        Just (Success (input', value)) -> Just (Success (input', Just value))

char :: Char -> Parser Char
char expected = Parser parse
  where
    parse :: String -> Maybe (Result Text (String, Char))
    parse []            = Nothing
    parse (actual:rest) = if actual == expected
        then Just (Success (rest, actual))
        else Nothing

text :: Text -> Parser Text
text expected = Parser parse
  where
    parse :: String -> Maybe (Result Text (String, Text))
    parse actual = case Data.List.stripPrefix (Data.Text.unpack expected) actual of
        Nothing   -> Nothing
        Just rest -> Just (Success (rest, expected))

range :: CharSet -> Parser Char
range expected = Parser parse
  where
    parse :: String -> Maybe (Result Text (String, Char))
    parse []            = Nothing
    parse (actual:rest) = if expected `contains` actual
        then Just (Success (rest, actual))
        else Nothing

data Eol = UNIX | Windows deriving (Eq, Ord, Show, Generic)

eol :: Parser Eol
eol = Parser parse
  where
    parse ('\r':'\n':rest) = Just (Success (rest, Windows))
    parse ('\n':rest)      = Just (Success (rest, UNIX))
    parse _                = Nothing

eof :: Parser ()
eof = Parser parse
  where
    parse input = if Data.List.null input
        then Just (Success (input, ()))
        else Nothing
