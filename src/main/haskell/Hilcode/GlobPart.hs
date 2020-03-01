module Hilcode.GlobPart
( GlobPart(..)
) where

import           Hilcode.CharSet
import           Hilcode.Nullable
import           Hilcode.Thread

import           TextShow (TextShow)
import qualified TextShow
import qualified TextShow.Data.Char

data GlobPart
    = Many
    | Any
    | Range CharSet
    | Single Char
    deriving (Eq, Ord, Show)

instance TextShow GlobPart where
    showb Many            = "*"
    showb Any             = "?"
    showb (Range charSet) = TextShow.showb charSet
    showb (Single ch)     = TextShow.Data.Char.showbLitChar ch

instance Nullable GlobPart where
    nullable Many       = True
    nullable Any        = False
    nullable (Range _)  = False
    nullable (Single _) = False

instance HasThread Char GlobPart where
    partMatcher ch     (Range charSet)   = charSet `contains` ch
    partMatcher actual (Single expected) = expected == actual
    partMatcher _      Any               = True
    partMatcher _      Many              = True
