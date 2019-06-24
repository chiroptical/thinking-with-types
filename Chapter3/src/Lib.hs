module Lib where

import Data.Functor.Contravariant
import Data.Functor.Invariant

-- Variance
-- ---
-- covariance: `a -> b` can be lifted into `T a -> T b`
-- contravariance: `a -> b` can be lifted into `T b -> T a`
-- invariant: no function `a -> b` can be lifted into a function
--            containing `T a`

--              | Position Of |
-- Type         |  a   |  b   |
-- ----------------------------
-- Either a b   |  +   |  +   |
-- (a, b)       |  +   |  +   |
-- a -> b       |  -   |  +   | 

--                       (+)               --> `a` is covariant in `T1 a`
newtype T1 a = T1 (Int -> a)

--                (-)                      --> `a` is contravariant in `T2 a`
newtype T2 a = T2 (a -> Int)

--                (-)  (+)                 --> `a` is invariant in `T3 a`
newtype T3 a = T3 (a -> a)

--                  -- (-) --              --> `a` is contravariant in `T4 a`
--                        (+)
newtype T4 a = T4 ((Int -> a) -> Int)

--                  -- (-) --              --> `a` is covariant in `T5 a`
--                 (-)
newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T1 where
  fmap f (T1 fInt) = T1 $ f . fInt

instance Contravariant T2 where
  contramap f (T2 fa) = T2 $ fa . f

-- QUESTION: How do we make this work?
-- Trick is, you need a fbb :: (b -> b) from an faa :: (a -> a) and f :: (a -> b)
instance Functor T3 where
  fmap f (T3 faa) = undefined

-- This comes from the package itself, but only works if you can
-- implement a Functor instance
-- invmapFunctor :: Functor f => (a -> b) -> (b -> a) -> f a -> f b
-- invmapFunctor = flip $ const fmap

instance Invariant T3 where
  invmap = undefined

instance Contravariant T4 where
  contramap f (T4 fIaI) = undefined

instance Functor T5 where
  -- f :: a -> b
  -- faII :: (a -> Int) -> Int
  -- result :: (b -> Int) -> Int
  fmap f (T5 faII) = undefined

-- `Either`  and `(,)` are covariate in both of their arguments,
-- these are called Bifunctors

-- `(->)` is contravariant in its first argument and covariate in its
-- second, this is known as a profunctor