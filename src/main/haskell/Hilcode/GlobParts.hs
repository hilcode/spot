module Hilcode.GlobParts
( GlobParts
, makeGlobParts
, matchGlobParts
) where

import           Hilcode.GlobPart
import           Hilcode.Nullable
import           Hilcode.Thread

import           Data.Text (Text)
import           Data.Vector (Vector)
import           TextShow (TextShow)
import qualified Data.Foldable
import qualified Data.Text
import qualified Data.Vector
import qualified TextShow

newtype GlobParts = GlobParts (Vector GlobPart)
    deriving (Eq, Ord, Show)

makeGlobParts :: Vector GlobPart -> GlobParts
makeGlobParts globParts
    = if Data.Vector.null globParts
        then error "A 'GlobParts' _must_ have at least 1 'GlobPart'."
        else GlobParts globParts

instance TextShow GlobParts where
    showb (GlobParts globParts) = Data.Foldable.foldl' (<>) "" (TextShow.showb <$> globParts)

instance Nullable GlobParts where
    nullable (GlobParts globParts) = Data.Foldable.all nullable globParts

matchGlobParts :: Text -> GlobParts -> Bool
matchGlobParts parts (GlobParts globParts) = match (Data.Text.unpack parts) globParts
