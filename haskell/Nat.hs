module Nat where

import Prelude hiding (Num(..))

data Nat where
    O :: Nat
    S :: Nat -> Nat
    deriving (Eq, Show)

(+) :: Nat -> Nat -> Nat
n + O = n
n + (S m) = S (n + m)
