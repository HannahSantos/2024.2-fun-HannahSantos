{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Polyfuns where

import Prelude hiding (Bool, True, False, fst, snd)

fst :: (a, b) -> a
fst (a, _) = a

snd :: (a, b) -> b
snd (_, b) = b

comp :: (b -> c) -> (a -> b) -> a -> c
comp g f a = g (f a)

(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) = comp