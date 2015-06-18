module FP02 where

import Data.Monoid (Monoid, mempty, mappend)

{- Monoid Laws

- associativity: mappend (mappend a b) c == mappend a (mappend b c)
- neural element: mappend mempty a == mappend a mempty == a

-}

instance Monoid Int where
  mempty = 0
  mappend = (+)
