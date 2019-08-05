{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Fcf where

import           Data.Kind                      ( Constraint
                                                , Type
                                                )

type Exp a = a -> Type

-- Open type family
type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b
type instance Eval (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval (FromMaybe _ ( 'Just a)) = a
type instance Eval (FromMaybe a 'Nothing) = a

-- Exercise 10.2-i
data ListToMaybe :: [a] -> Exp (Maybe a)
type instance Eval (ListToMaybe (a ': '[])) = 'Just a
type instance Eval (ListToMaybe '[]) = 'Nothing
type instance Eval (ListToMaybe (_ ': _ ': _)) = 'Nothing

data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)

-- Exercise 10.2-ii
data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr f acc '[]) = acc
type instance Eval (Foldr f acc (a ': as)) = Eval (f a (Eval (Foldr f acc as)))

data Pure :: a -> Exp a
type instance Eval (Pure x) = x

-- Behaves like function application `($)`
data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))
infixr 0 =<<

-- Behaves like composition `(.)`
data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))
infixr 1 <=<

data TyEq :: a -> b -> Exp Bool
-- `TyEq a b` and `TyEq a a` lack apartness
-- `a` can be substituted for `b` and vice-versa
-- Therefore, we use a closed type family to
-- regain ordering between instances
type instance Eval (TyEq a b) = TyEqImpl a b
-- closed type family
type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

data Collapse :: [Constraint] -> Exp Constraint
type instance Eval (Collapse '[]) = (() :: Constraint)
type instance Eval (Collapse (a ': as)) = (a, Eval (Collapse as))

type All (c :: k -> Constraint) (ts :: [k]) = Collapse =<< MapList (Pure1 c) ts

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x
