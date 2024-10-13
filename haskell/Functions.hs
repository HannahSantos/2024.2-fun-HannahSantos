{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Functions where

import Prelude hiding (Num(..), Bool, True, False, (<), div)
import Nat 
import Bool

(<) :: Nat -> Nat -> Bool
_ < O = False
O < _ = True
(S n) < (S m) = n < m

(≤) :: Nat -> Nat -> Bool
O ≤ _ = True
_ ≤ O = False
(S n) ≤ (S m) = n ≤ m

ev :: Nat -> Bool
ev O = True
ev (S n) = od n

od :: Nat -> Bool
od O = False
od (S n) = ev n

if_then_else :: Bool -> a -> a -> a
if_then_else True a b = a
if_then_else False a b = b

