module List where

import Nat
import Prelude hiding (length)

data List a where
    Nil :: List a
    Cons :: a -> List a -> List a
    deriving (Eq, Show)

length :: List a -> Nat
length Nil = O
length (Cons x xs) = S (length xs)