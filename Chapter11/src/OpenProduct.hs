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

module OpenProduct where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Fcf
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

-- Key allows users to provide labels for data
-- e.g `Key @"myData"` is a value whose type is `Key "myData"`

-- insert :: Key key -> f t -> OpenProduct f ts -> OpenProduct f ('(key, t) ': ts)
-- insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

-- null $ filter ((==key) . fst) [(keys, _)]
type UniqueKey (key :: k) (ts :: [(k, t)]) = Null =<< Filter (TyEq key <=< Fst) ts

-- We can use UniqueKey to ensure keys are not duplicated
insert :: Eval (UniqueKey key ts) ~ True => Key key -> f t -> OpenProduct f ts -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

type LookupType (key :: k) (ts :: [(k, t)]) = FromMaybe Stuck =<< Lookup key ts

get :: forall key ts f. KnownNat (FindElem key ts)
    => Key key
    -> OpenProduct f ts
    -> f (Eval (LookupType key ts))
get _ (OpenProduct v) = unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  SetIndex (FindElem key ts) '(key, t) ts

update :: forall key ts t f. KnownNat (FindElem key ts)
      => Key key
      -> f t
      -> OpenProduct f ts
      -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) = OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

type DeleteElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) = Filter (TyEq key <=< Fst) ts

delete :: forall key ts t f. KnownNat (FindElem key ts)
      => Key key
      -> f t
      -> OpenProduct f ts
      -> OpenProduct f (Eval (DeleteElem key t ts))
delete _ ft (OpenProduct v) = OpenProduct $ V.drop (findElem @key @ts) v

-- upsert :: forall key ts t f. KnownNat (FindElem key ts)
--       => Key key
--       -> f t
--       -> OpenProduct f ts
--       -> OpenProduct f (Eval (??? key t ts))
-- upsert _ ft (OpenProduct v) = undefined
