module Hilcode.Result where

import GHC.Generics

data Result failure success
    = Success success
    | Failure failure
    deriving (Eq, Ord, Show, Generic)

instance Functor (Result failure) where
    fmap _ (Failure failure) = Failure failure
    fmap f (Success value)   = Success (f value)

instance Semigroup (Result failure success) where
    Failure _ <> rgt = rgt
    lft       <> _   = lft

instance Monoid success => Monoid (Result failure success) where
    mempty = mempty

instance Applicative (Result failure) where
    pure = Success
    Failure failure <*> _      = Failure failure
    Success f       <*> result = f <$> result

instance Monad (Result failure) where
    Failure failure >>= _ = Failure failure
    Success value   >>= f = f value

instance Foldable (Result failure) where
    foldMap _ (Failure _)     = mempty
    foldMap f (Success value) = f value

    foldr _ zero (Failure _)     = zero
    foldr f zero (Success value) = f value zero

    length (Failure _) = 0
    length (Success _) = 1

    null = isFailure

instance Traversable (Result failure) where
    traverse _ (Failure failure) = pure (Failure failure)
    traverse f (Success value)   = Success <$> f value

isFailure :: Result failure success -> Bool
isFailure (Failure _) = True
isFailure (Success _) = False

isSuccess :: Result failure success -> Bool
isSuccess (Failure _) = False
isSuccess (Success _) = True
