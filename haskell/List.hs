module List where

import Nat
import Prelude 
    hiding (Num(..), length, elem, sum, product, (++), reverse,
            all)

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

(++) :: List a -> List a -> List a
Nil ++ ys = ys
(Cons x xs) ++ ys = Cons x (xs ++ ys)

append :: a -> List a -> List a
append x Nil = Cons x Nil
append x (Cons y ys) = Cons y (append x ys)

reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons x xs) = append x (reverse xs)

all :: (a -> Bool) -> List a -> Bool
all _ Nil = True
all p (Cons x xs)
    | p x         = all p xs
    | otherwise   = False