module Hilcode.Misc
( isFirstOf
, lift
, removeDuplicates
, separate
, showTextList
) where

import Data.Text (Text)

lift :: (Monad monad, Applicative applicative) => monad (a -> b) -> monad (applicative a) -> monad (applicative b)
lift mf mv = do
    f <- mf
    v <- mv
    pure (f <$> v)

isFirstOf :: Eq a => a -> [a] -> Bool
isFirstOf _ []                = False
isFirstOf expected (actual:_) = expected == actual

separate :: forall a . Eq a => a -> [a] -> [[a]]
separate separator as = go (removeDuplicates separator as) [] []
  where
    go [] as' as'' = as'' <> [as']
    go [a] as' as'' = if a == separator then as'' <> [as'] else as'' <> [as' <> [a]]
    go (a:as) as' as'' = if a == separator
        then go as [] (as'' <> [as'])
        else go as (as' <> [a]) as''

removeDuplicates :: Eq a => a -> [a] -> [a]
removeDuplicates _ [] = []
removeDuplicates _ [a] = [a]
removeDuplicates a (a1:a2:as)
    | a1 == a && a2 == a = rest
    | otherwise          = a1 : rest
  where
    rest = removeDuplicates a (a2:as)

showTextList :: [Text] -> Text
showTextList ts = "[" <> go ts <> "]"
  where
    go []     = ""
    go [t]    = t
    go (t:ts) = t <> ", " <> go ts