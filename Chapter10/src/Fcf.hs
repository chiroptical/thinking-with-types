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

data Map :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

type instance Eval (Map f 'Nothing) = 'Nothing
type instance Eval (Map f ('Just x)) = 'Just (Eval (f x))

type instance Eval (Map f ('Left e)) = 'Left e
type instance Eval (Map f ('Right x)) = 'Right (Eval (f x))

-- 10.4-i, Write a promoted Functor instance for tuples
type instance Eval (Map f '(x, y)) = '(x, Eval (f x))

data (++) :: [a] -> [a] -> Exp [a]
type instance Eval ((++) '[] bs) = bs
type instance Eval ((++) (a ': as') bs) = a ': Eval ((++) as' bs)

data Mappend :: a -> a -> Exp a
type instance Eval (Mappend '() '()) = '()
type instance Eval (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)
type instance Eval (Mappend (a :: [k]) (b :: [k])) = Eval (a ++ b)

-- The (Constraint, Constraint) is actually a logical AND and is a syntactical quirk
-- Above we can't use '(a, b) because that represents a tuple

-- This is one way to embed Mempty in the type system
data Mempty :: k -> Exp k
type instance Eval (Mempty '()) = '()
type instance Eval (Mempty (k :: Constraint)) = (() :: Constraint)
type instance Eval (Mempty '[k]) = '[]

-- The below code uses a feature of the compiler to
-- reduce the amount of boilerplate above
data Mempty' :: Exp k
type instance Eval (Mempty' :: Exp ()) = '()
type instance Eval (Mempty' :: Exp Constraint) = ()
type instance Eval (Mempty' :: Exp [k]) = '[]
