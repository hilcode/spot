module Hilcode.Internal.Queue where

import qualified Control.Applicative

data Queue a
    = Queue [a] [a]
    deriving Show

instance Eq a => Eq (Queue a) where
    (Queue []                 [])       == (Queue []                 [])       = True
    (Queue []                 [])       == (Queue (_:_)              (_:_))    = False
    (Queue (_:_)              (_:_))    == (Queue []                 [])       = False
    (Queue lftHeads           [])       == (Queue rgtHeads           [])       = lftHeads == rgtHeads
    (Queue []                 lftTails) == (Queue []                 rgtTails) = lftTails == rgtTails
    (Queue lftHeads           [])       == (Queue []                 rgtTails) = lftHeads == reverse rgtTails
    (Queue []                 lftTails) == (Queue rgtHeads           [])       = reverse lftTails == rgtHeads
    (Queue []                 lftTails) == (Queue rgtHeads           rgtTails) = reverse lftTails == rgtHeads <> reverse rgtTails
    (Queue lftHeads           lftTails) == (Queue []                 rgtTails) = lftHeads <> reverse lftTails == reverse rgtTails
    (Queue (lftHead:lftHeads) lftTails) == (Queue (rgtHead:rgtHeads) rgtTails) = lftHead == rgtHead && (Queue lftHeads lftTails) == (Queue rgtHeads rgtTails)

push :: a -> Queue a -> Queue a
push a (Queue heads tails) = Queue heads (a:tails)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue []           [])    = Nothing
pop (Queue (head:heads) tails) = Just (head, Queue heads tails)
pop (Queue []           tails) = pop (Queue (reverse tails) [])

empty :: Queue a
empty = mempty

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

singleton :: a -> Queue a
singleton a = Queue [a] []

fromList :: [a] -> Queue a
fromList heads = Queue heads []

toList :: Queue a -> [a]
toList (Queue heads tails) = heads <> reverse tails

instance Semigroup (Queue a) where
    lftQueue <> rgtQueue = Queue (toList lftQueue <> toList rgtQueue) []

instance Monoid (Queue a) where
    mempty = Queue [] []

instance Functor Queue where
    fmap f (Queue heads tails) = Queue (f <$> heads) (f <$> tails)

instance Applicative Queue where
    pure = singleton
    liftA2 f lftQueue rgtQueue = Queue (Control.Applicative.liftA2 f (toList lftQueue) (toList rgtQueue)) []

instance Foldable Queue where
    foldr _  zero (Queue []    [])    = zero
    foldr op zero (Queue heads [])    = foldr op zero heads
    foldr op zero (Queue []    tails) = foldr op zero (reverse tails)
    foldr op zero queue               = foldr op zero (toList queue)

instance Monad Queue where
    return = pure
    queue >>= f = mconcat (f <$> toList queue)
