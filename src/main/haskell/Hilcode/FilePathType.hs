module Hilcode.FilePathType
( FilePathType(..)
) where

import           TextShow (TextShow)
import qualified TextShow

data FilePathType
    = Relative | Absolute
    deriving (Eq, Ord, Show)

instance TextShow FilePathType where
    showb Relative = "Relative"
    showb Absolute = "Absolute"
