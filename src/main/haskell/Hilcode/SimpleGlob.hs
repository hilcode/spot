module Hilcode.SimpleGlob
( SimpleGlob
, makeSimpleGlob
, matchSimpleGlob
) where

import           Hilcode.GlobParts
import           Hilcode.Nullable

import           Data.Text (Text)
import           Data.Vector (Vector)
import           TextShow (TextShow)
import qualified Data.Foldable
import qualified Data.List
import qualified Data.Vector
import qualified TextShow

newtype SimpleGlob
    = SimpleGlob (Vector GlobParts)
    deriving (Eq, Ord, Show)

makeSimpleGlob :: Vector GlobParts -> SimpleGlob
makeSimpleGlob globPartsVector
    = if Data.Vector.null globPartsVector
        then error "A 'SimpleGlob' _must_ have at least 1 'GlobParts'."
        else SimpleGlob globPartsVector

instance TextShow SimpleGlob where
    showb (SimpleGlob globParts) = Data.Foldable.foldl' (<>) mempty (Data.List.intersperse (TextShow.singleton '|') (Data.Vector.toList (TextShow.showb <$> globParts)))

instance Nullable SimpleGlob where
    nullable (SimpleGlob globParts) = Data.Vector.null globParts || Data.Foldable.any nullable globParts

matchSimpleGlob :: Text -> SimpleGlob -> Bool
matchSimpleGlob parts (SimpleGlob globParts) = Data.Foldable.or (matchGlobParts parts <$> globParts)
