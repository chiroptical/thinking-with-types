{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import           Data.Proxy
import           GHC.TypeLits

a :: Int
a = 4

b :: Int
b = 5

-- Exercise 2.1.3-i
-- If `Show Int` has kind _CONSTRAINT_, what is the kind of `Show`?
-- :k Show :: * -> Constraint

-- Exercise 2.1.3-ii
-- What is the kind of `Functor`?
-- :k Functor :: * -> * -> *
-- The above is incorrect because Functor isn't a type it is a _CONSTRAINT_
-- :k Functor :: (* -> *) -> Constraint

-- Exercise 2.1.3-iii
-- What is the kind of Monad
-- :k Monad :: (* -> *) -> Constraint

-- Exercise 2.1.3-iv
-- What is the kind of MonadTrans
-- Requires `transformers` package
-- import Control.Monad.Trans.Class
-- :k MonadTrans :: ((* -> *) -> * -> *) -> Constraint

-- Discussing DataKinds Language Extension

-- Promoted data constructors are build 'Up,'Down w/ kind Spin
data Spin = Up | Down

-- Promoted data constructor 'Unit
data Unit = Unit

c :: Maybe Unit
c = undefined

-- But this fails! :k Maybe :: TYPE -> TYPE, but :k 'Unit :: Unit
-- d :: Maybe 'Unit
-- d = undefined

-- If you use the UserType, you can call
-- sensitive things as a User, however
-- using the Maybe (Proxy 'Admin) you
-- can get a type error when trying to
-- call doSensitiveThings as a User

-- data UserType = User | Admin
data Admin = Admin
data User =
  User
    { userAdminToken :: Maybe (Proxy 'Admin)
    }

-- doSensitiveThings :: User -> IO ()
-- doSensitiveThings User = undefined
-- doSensitiveThings Admin = undefined

doSensitiveThings :: Proxy 'Admin -> IO ()
doSensitiveThings = undefined

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True _ = 'True
  Or 'False y = y

-- Exercise 2.4-i
-- Write the closed type family for `Not`

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not _ = 'True

-- Although this type family is written correctly,
-- it isn't particularly useful because kinds must
-- be fully saturated. i.e.
-- Map (Or 'True) '[ 'True, 'False]
-- will fail because Or is partially saturated

type family Map (x :: a -> b) (i :: [a]) :: [b] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

-- What is the kind of Foo?
-- :k Foo :: Bool -> Bool -> Bool
type family Foo (x :: Bool) (y :: Bool) :: Bool

-- What is the kind of Bar?
-- :k Bar :: Type -> Type -> Bool -> Bool -> Bool
type family Bar x y :: Bool -> Bool -> Bool
