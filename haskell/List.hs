module List where

import Nat
import Prelude 
    hiding (Num(..), length, elem, sum, product)

data List a where
    Nil :: List a
    Cons :: a -> List a -> List a
    deriving (Eq, Show)

length :: List a -> Nat
length Nil = O
length (Cons x xs) = S (length xs)

elem :: Eq a => a -> List a -> Bool
elem _ Nil = False
elem n (Cons x xs)
    | n == x    = True
    | otherwise = elem n xs

sum :: List Nat -> Nat
sum Nil = O
sum (Cons x xs) = x + sum xs

product :: List Nat -> Nat
product Nil = S O
product (Cons x xs) = x * product xs