module List where

import Nat
import Prelude 
    hiding (Num(..), map, length, elem, sum, product, (++), reverse,
            all, any, take, drop, enumFrom)

data List a where
    Nil :: List a
    Cons :: a -> List a -> List a
    deriving (Eq, Show)

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

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

any :: (a -> Bool) -> List a -> Bool
any _ Nil = False
any p (Cons x xs)
    | p x         = True
    | otherwise   = any p xs

take :: Nat -> List a -> List a
take (S n) (Cons x xs) = Cons x (take n xs)
take _ _ = Nil

drop :: Nat -> List a -> List a
drop (S n) (Cons x xs) = drop n xs
drop _ xs = xs

enumFrom :: Nat -> List Nat
enumFrom n = Cons n (enumFrom (S n))