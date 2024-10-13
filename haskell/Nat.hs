module Nat where

import Prelude hiding (Num(..), (^), min, max)

data Nat where
    O :: Nat
    S :: Nat -> Nat
    deriving (Eq, Show)

(+) :: Nat -> Nat -> Nat
n + O = n
n + (S m) = S (n + m)

(*) :: Nat -> Nat -> Nat
n * O = O
n * (S m) = (n * m) + n

(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ (S m) = (n ^ m) * n

(∸) :: Nat -> Nat -> Nat
(S n) ∸ (S m) = n ∸ m
n ∸ _ = n

double :: Nat -> Nat
double = (*) (S (S O))

pred :: Nat -> Nat
pred O = O
pred (S n) = n

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib (S (S n)) = fib (S n) + fib n
fib n = n

min :: (Nat, Nat) -> Nat
min (S n, S m) = S (min (n, m))
min (n, m) = O

max :: (Nat, Nat) -> Nat
max (n, O) = n
max (O, m) = m
max (S n, S m) = S (max (n, m))