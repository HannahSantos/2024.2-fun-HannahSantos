{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Functions where

import Prelude 
    hiding (Num(..), Bool, True, False, (<), fst, snd, div, quot, rem)
import Polyfuns
import Nat 
import Bool

if_then_else :: Bool -> a -> a -> a
if_then_else True a _ = a
if_then_else False _ b = b

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

zero :: Nat -> Bool
zero O = True
zero _ = False

div :: (Nat, Nat) -> (Nat, Nat)
div (_, O) = error "Zero cannot go there, your computer might break."
div (n, m) = if_then_else (n < m) 
            (O, n) 
            (let (q', r') = div (n ∸ m, m)
                  in (S q', r'))

quot :: (Nat, Nat) -> Nat
quot = fst ∘ div

rem :: (Nat, Nat) -> Nat
rem = snd ∘ div