module List where

import Prelude hiding ()

data List a where
    Nil :: List a
    Cons :: a -> List a -> List a
    deriving (Eq, Show)
