module Hilcode.PathComponentGlob
( PathComponentGlob(..)
, partMatcher
) where

import           Hilcode.Nullable
import           Hilcode.SimpleGlob
import           Hilcode.Thread

import           Data.Text (Text)
import           TextShow (TextShow)
import qualified TextShow

data PathComponentGlob
    = PathComponentGlob SimpleGlob
    | ManyDirectories
    deriving (Eq, Ord, Show)

instance TextShow PathComponentGlob where
    showb (PathComponentGlob simpleGlob) = TextShow.showb simpleGlob
    showb ManyDirectories                = TextShow.fromText "**"

instance Nullable PathComponentGlob where
    nullable (PathComponentGlob simpleGlob) = nullable simpleGlob
    nullable ManyDirectories                = True

instance HasThread Text PathComponentGlob where
    partMatcher part (PathComponentGlob simpleGlob) = matchSimpleGlob part simpleGlob
    partMatcher _    ManyDirectories                = True
