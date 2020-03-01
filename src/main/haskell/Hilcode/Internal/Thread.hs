module Hilcode.Internal.Thread where

import           Hilcode.Nullable

import           Data.Set (Set)
import           Data.Vector (Vector)
import qualified Data.Foldable
import qualified Data.Set
import qualified Data.Vector

class HasThread part matcher where
    partMatcher :: part -> matcher -> Bool

data Thread part matcher
    = Thread [part] [matcher]
    | Success
    deriving (Eq, Ord, Show)

isNullable :: Nullable nullable => [nullable] -> Bool
isNullable nullables = Data.Foldable.foldl' (&&) True (nullable <$> nullables)

match :: forall part matcher . (Ord part, Ord matcher, Nullable matcher, HasThread part matcher) => [part] -> Vector matcher -> Bool
match parts matchers = go (Data.Set.singleton (Thread parts (Data.Vector.toList matchers)))
  where
    go :: Set (Thread part matcher) -> Bool
    go threads
        | Data.Set.null threads             = False
        | Success `Data.Set.member` threads = True
        | otherwise                         = go (step threads)

step :: (Ord part, Ord matcher, Nullable matcher, HasThread part matcher) => Set (Thread part matcher) -> Set (Thread part matcher)
step = Data.Foldable.foldMap stepThread

stepThread :: forall part matcher . (Ord part, Ord matcher, Nullable matcher, HasThread part matcher) => Thread part matcher -> Set (Thread part matcher)
stepThread Success                                  = Data.Set.singleton Success
stepThread (Thread []             matchers)         = if isNullable matchers then Data.Set.singleton Success else Data.Set.empty
stepThread (Thread (_:_)          [])               = Data.Set.empty
stepThread (Thread allParts@(part:parts) allMatchers@(matcher:matchers))
    | partMatcher part matcher = if nullable matcher
        then Data.Set.fromList [Thread parts matchers, Thread allParts matchers, Thread parts allMatchers]
        else Data.Set.singleton (Thread parts matchers)
    | otherwise                = if nullable matcher
        then Data.Set.singleton (Thread allParts matchers)
        else Data.Set.empty
