{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- Heterogenous List
module HList where

import           Data.Kind    (Constraint, Type)
import           GHC.TypeLits (TypeError)
import           GHC.TypeLits (ErrorMessage (Text))

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (h :# _) = h

hTail :: HList (t ': ts) -> HList ts
hTail (_ :# ts) = ts

-- Original, non-ideal, implementation of hLast
-- data Any = forall a. Any a
-- hLast :: HList (t ': ts) -> Any
-- hLast (t :# HNil) = Any t
-- hLast (_ :# t :# ts) = hLast (t :# ts)

type family Last (ts :: [Type]) :: Type where
  Last '[] = TypeError (Text "Empty lists don't have a Last type")
  Last (t ': '[]) = t
  Last (t ': ts) = Last ts

hLast :: HList ts -> Last ts
hLast HNil               = error "hLast bottoms for empty list"
hLast (t :# HNil)        = t
hLast (_ :# ts@(_ :# _)) = hLast ts

showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

-- instance Eq (HList '[]) where
--   HNil == HNil = True

-- instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--   (a :# as) == (b :# bs) = a == b && as == bs

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

-- type family AllEq (ts :: [Type]) :: Constraint where
--   AllEq '[] = ()
--   AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (t :# ts) = show t <> " :# " <> show ts
  -- Due to the Haskell monomorphism restriction,
  -- -> one cannot use where (or let) to keep `t` and `ts`
  -- -> as the same types in the signature
  -- -> and therefore `go` (below) can't compile
  -- -> **Need to learn a bit more about this**
  -- show (t :# ts) = "[" ++ go (t :# ts) ++ "]"
  --   where
  --     go (t :# ts) = show t ++ "," ++ go ts

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  HNil `compare` HNil = EQ
  (t :# ts) `compare` (t' :# ts') = t `compare` t' <> ts `compare` ts'