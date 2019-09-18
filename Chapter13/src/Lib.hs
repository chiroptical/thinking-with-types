{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import           GHC.Generics

-- instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
--   F0 == F0 = True
--   F1 a1 == F1 a2 = a1 == a2
--   F2 b1 c1 == F2 b2 c2 = b1 == b2 && c1 == c2

data Maybe' a = Just' a | Nothing'

toCanonical :: Maybe a -> Either () a
toCanonical Nothing  = Left ()
toCanonical (Just a) = Right a

fromCanonical :: Either () a -> Maybe a
fromCanonical (Left  ()) = Nothing
fromCanonical (Right a ) = Just a

-- :info Bool
-- data Bool = False | True
-- :kind! Rep Bool
-- Rep Bool
--   = ... 
--       (   ... U1
--       :+: ... U1
--       )

-- Three-fold approach to generically deriving structural polymorphism
--
-- 1. Define a typeclass which acts as a _carrier_, prefix typeclass with `G`
class GEq a where
  geq :: a x -> a x -> Bool

-- 2. Provide inductive instances of the class for generic data constructors
instance GEq U1 where
  geq _ _ = True

instance GEq V1 where
  geq _ _ = True

instance Eq a => GEq (K1 _1 a) where
  geq (K1 a) (K1 b) = a == b

instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a1) (L1 a2) = geq a1 a2
  geq (R1 b1) (R1 b2) = geq b1 b2
  geq _       _       = False

instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

instance GEq a => GEq (M1 _x _y a) where
  geq (M1 a1) (M1 a2) = geq a1 a2

-- 3. Write a helper function to map between `Rep` and the desired type
genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

-- instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
--   (==) = genericEq

-- Exercise 13.2-i: Provide a Generic instance for `Ord`
class GOrd a where
  gcompare :: a x -> a x -> Ordering

-- U1 ~ ()
instance GOrd U1 where
  gcompare _ _ = EQ

instance GOrd V1 where
  gcompare _ _ = EQ

instance Ord a => GOrd (K1 _1 a) where
  gcompare (K1 a) (K1 b) = compare a b

instance (GOrd a, GOrd b) => GOrd (a :+: b) where
  gcompare (L1 a1) (L1 a2) = gcompare a1 a2
  gcompare (R1 a1) (R1 a2) = gcompare a1 a2
  gcompare (L1 _ ) (R1 _ ) = LT
  gcompare (R1 _ ) (L1 _ ) = GT

instance (GOrd a, GOrd b) => GOrd (a :*: b) where
  gcompare (a1 :*: b1) (a2 :*: b2) = gcompare a1 a2 <> gcompare b1 b2

instance GOrd a => GOrd (M1 _x _y a) where
  gcompare (M1 a1) (M1 a2) = gcompare a1 a2

genericOrd :: (Generic a, GOrd (Rep a)) => a -> a -> Ordering
genericOrd a b = gcompare (from a) (from b)

-- Exercise 13.2-ii Use GHC.Generics to implement `exNihilo :: Maybe a`

data A = A deriving (Show, Generic)

class GExNihilo a where
  gExNihilo :: Maybe (a x)

instance GExNihilo U1 where
  gExNihilo = Just U1

instance GExNihilo V1 where
  gExNihilo = Nothing

instance GExNihilo (K1 _1 a) where
  gExNihilo = Nothing

instance GExNihilo (a :+: b) where
  gExNihilo = Nothing

instance GExNihilo (a :*: b) where
  gExNihilo = Nothing

instance GExNihilo a => GExNihilo (M1 _x _y a) where
  gExNihilo = M1 <$> gExNihilo

exNihilo :: (Generic a, GExNihilo (Rep a)) => Maybe a
exNihilo = to <$> gExNihilo

class MyEq a where
  eq :: a -> a -> Bool

  default eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
  eq a b = geq (from a) (from b)

data Foo a b c = F0 | F1 a | F2 b c deriving Generic
