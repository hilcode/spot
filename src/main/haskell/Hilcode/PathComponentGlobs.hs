module Hilcode.PathComponentGlobs
( PathComponentGlobs
, makePathComponentGlobs
, matchTextsToPathComponentGlobs
) where

import           Hilcode.Nullable
import           Hilcode.PathComponentGlob
import           Hilcode.Thread

import           Data.Text (Text)
import           Data.Vector (Vector)
import           TextShow (TextShow)
import qualified Data.Foldable
import qualified Data.List
import qualified Data.Vector
import qualified TextShow
import qualified TextShow.Data.Char

newtype PathComponentGlobs
    = PathComponentGlobs (Vector PathComponentGlob)
    deriving (Eq, Show)

makePathComponentGlobs :: Vector PathComponentGlob -> PathComponentGlobs
makePathComponentGlobs pathComponentGlobs
    = if Data.Vector.null pathComponentGlobs
        then error "A 'PathComponentGlobs' _must_ have at least 1 'PathComponentGlob'."
        else PathComponentGlobs pathComponentGlobs

instance TextShow PathComponentGlobs where
    showb (PathComponentGlobs pathComponentGlobs)
        = Data.Foldable.foldl' (<>) mempty (Data.List.intersperse (TextShow.Data.Char.showbLitChar '/') (Data.Vector.toList (TextShow.showb <$> pathComponentGlobs)))

instance Nullable PathComponentGlobs where
    nullable (PathComponentGlobs pathComponentGlobs) = Data.Foldable.all nullable pathComponentGlobs

matchTextsToPathComponentGlobs :: [Text] -> PathComponentGlobs -> Bool
matchTextsToPathComponentGlobs parts (PathComponentGlobs pathComponentGlobs) = match parts pathComponentGlobs
