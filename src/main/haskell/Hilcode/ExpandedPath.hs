module Hilcode.ExpandedPath
( ExpandedPath(..)
, toExpandedPath
) where

import           Data.Text (Text)
import           TextShow (TextShow)
import qualified Data.Text
import qualified TextShow

import           System.Path (Path)
import           System.Path.PartClass (AbsRel, FileOrDir)
import qualified System.Path

data ExpandedPath
    = AbsoluteDirectory [Text]
    | RelativeDirectory [Text]
    | AbsoluteFile      [Text]
    | RelativeFile      [Text]
    deriving (Eq, Ord, Show)

toPathBuilder :: [Text] -> TextShow.Builder
toPathBuilder []                         = TextShow.singleton '/'
toPathBuilder [filePart]                 = TextShow.fromText filePart
toPathBuilder (filePart:fileParts@(_:_)) = TextShow.fromText filePart <> TextShow.singleton '/' <> toPathBuilder fileParts

instance TextShow ExpandedPath where
    showb :: ExpandedPath -> TextShow.Builder
    showb (AbsoluteDirectory [])        = toPathBuilder []
    showb (AbsoluteDirectory fileParts) = TextShow.singleton '/' <> toPathBuilder fileParts <> TextShow.singleton '/'
    showb (RelativeDirectory fileParts) = toPathBuilder fileParts <> TextShow.singleton '/'
    showb (RelativeFile      fileParts) = toPathBuilder fileParts
    showb (AbsoluteFile      fileParts) = TextShow.singleton '/' <> toPathBuilder fileParts

toExpandedPath :: (AbsRel ar, FileOrDir fd) => Path ar fd -> ExpandedPath
toExpandedPath path = case System.Path.splitPath path of
    (True,  relDirs, Nothing)      -> AbsoluteDirectory (Data.Text.pack <$> (System.Path.toString <$> relDirs))
    (True,  relDirs, Just relFile) -> AbsoluteFile      (Data.Text.pack <$> ((System.Path.toString <$> relDirs) <> [System.Path.toString relFile]))
    (False, relDirs, Nothing)      -> RelativeDirectory (Data.Text.pack <$> (System.Path.toString <$> relDirs))
    (False, relDirs, Just relFile) -> RelativeFile      (Data.Text.pack <$> ((System.Path.toString <$> relDirs) <> [System.Path.toString relFile]))
