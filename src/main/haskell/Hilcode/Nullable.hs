module Hilcode.Nullable
( Nullable
, nullable
) where

class Nullable a where
    nullable :: a -> Bool
