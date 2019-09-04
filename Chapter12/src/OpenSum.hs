{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenSum where

import Data.Kind (Type)
import Data.Proxy
import Fcf
import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

-- ts ~ '[ Int, Bool, String], t is extistentially an Int, Bool, or String but
-- we don't know which one
--
-- OpenSum ((->) String) '[Int, Bool] is capable of storing String -> Int
-- and String -> Bool
--
-- The Int is used to "remember" the type t had. e.g. If the Int is 2 and
-- ts ~ '[A, B, C, D] then t has type C.

-- FindElem is an alias for a first class type family,
-- "first class" means the type family is just a type
-- Remember =<< operates like function application
-- findIndex :: (a -> Bool) -> [a] -> Maybe Int
-- TyEq is partially applied to key, i.e. (key==)
-- Stuck behaves like a compile time undefined or "bottom" kind
-- because the compiler can't reduce the type family further
-- it will simply give up
type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

type family FriendlyFindElem (f :: k -> Type) (t :: k) (ts :: [k]) where
  FriendlyFindElem f t ts =
    FromMaybe ( TypeError
                (     'Text "Attemped to call `friendlyProject' to produce a `"
                ':<>: 'ShowType (f t)
                ':<>: 'Text "'."
                ':$$: 'Text "But the OpenSum can only contain one of: "
                ':$$: 'Text ""
                ':<>: 'ShowType ts
                )
              ) =<< FindIndex (TyEq t) ts

-- This type alias is used to help GHC realize
-- the result of FindElem is a Natural Number
type Member t ts = KnownNat (Eval (FindElem t ts))
type Member' f t ts = KnownNat (Eval (FriendlyFindElem f t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

inject :: forall f t ts. Member t ts => f t -> OpenSum f ts
inject = UnsafeOpenSum (findElem @t @ts)

project :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
project (UnsafeOpenSum i f) = if i == findElem @t @ts
                                 then Just $ unsafeCoerce f
                                 else Nothing

friendlyProject :: forall f t ts. Member' f t ts => OpenSum f ts -> Maybe (f t)
friendlyProject (UnsafeOpenSum i f) = Just $ unsafeCoerce f

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

weaken :: OpenSum f ts -> OpenSum f (t ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

match :: forall f ts b. (forall t. f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t
