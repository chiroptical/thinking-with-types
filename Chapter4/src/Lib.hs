{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}


module Lib where

import Data.Typeable

-- Hindley-Milner yields the ability to infer types
-- of programs without specifying them!

-- This is broken because `apply :: b` is not the same `b`!
-- broken :: (a -> b) -> a -> b
-- broken f a = apply
--   where
--     apply :: b
--     apply = f a

-- This works because ScopedTypeVariables introduces a type scope
-- for `a` and `b` and exposes them to the rest of the function
fixed :: forall a b. (a -> b) -> a -> b
fixed f a = apply
  where
    apply :: b
    apply = f a

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

type family AlwaysUnit a where
  AlwaysUnit a = ()

-- Are all of the type signatures non-ambiguous

f :: AlwaysUnit a -> a
f = undefined

g :: b -> AlwaysUnit a -> b
g = undefined

-- This one is ambiguous because we don't know which
-- `a` to use for `Show a`, unless we use AllowAmbiguousTypes

h :: Show a => AlwaysUnit a -> String
h = undefined