module Functions where

import Prelude hiding (Num(..), Bool, True, False, (<))
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