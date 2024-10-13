module Bool where

import Prelude hiding (Bool, True, False)

data Bool where
    True :: Bool
    False :: Bool
    deriving (Eq, Show)

band :: (Bool, Bool) -> Bool
band (True, True) = True
band (_, _) = False

bor :: (Bool, Bool) -> Bool
bor (False, False) = False
bor (_, _) = True

bxor :: (Bool, Bool) -> Bool
bxor (True, False) = True
bxor (False, True) = True
bxor (_, _) = False

bnot :: Bool -> Bool
bnot True = False
bnot False = True

bnand :: (Bool, Bool) -> Bool
bnand = bnot . band

bnor :: (Bool, Bool) -> Bool
bnor = bnot . bor

bxnor :: (Bool, Bool) -> Bool
bxnor = bnot . bxor