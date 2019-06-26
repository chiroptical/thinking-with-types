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

type BHList = HList '[Bool]

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

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

f = (True :# 1 :# HNil) == (False :# 2 :# HNil)
-- This is a type error!
-- g = (True :# 1 :# HNil) == (False :# 2 :# 4 :# HNil)