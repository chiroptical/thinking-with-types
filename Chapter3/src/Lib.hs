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

-- A Covariant only has positive occurences and is a valid Invariant
-- A Contravariant only has negative occurences and is a valid Invariant
-- A Phantom has neither positive/negative occurences and is a valid Invariant
-- An Invariant is _only_ Invariant when it contains both positive/negative occurences

-- Important Note: the `invariant` package implements Invariant instances
-- for data types which are already Functors (Covariant). `T3` is __only__
-- Invariant and therefore the function (http://hackage.haskell.org/package/invariant-0.5.3/docs/src/Data.Functor.Invariant.html#invmapFunctor)
-- will not work for T3.

instance Functor T1 where
  fmap f (T1 fia) = T1 $ f . fia

instance Contravariant T2 where
  contramap f (T2 fai) = T2 $ fai . f

-- T3 is the Endomorphism (https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html#t:Endo)
instance Invariant T3 where
  invmap fab fba (T3 faa) = T3 $ fab . faa . fba

instance Contravariant T4 where
  -- contramap f (T4 fibi) = T4 $ \ib -> fibi $ f . ib
  contramap f (T4 fiai) = T4 $ fiai . (f .)

instance Functor T5 where
  -- fmap f (T5 faii) = T5 $ \bi -> faii $ bi . f
  fmap f (T5 faii) = T5 $ faii . (. f)

-- `Either`  and `(,)` are covariate in both of their arguments,
-- these are called bifunctors

-- `(->)` is contravariant in its first argument and covariate in its
-- second, this is known as a profunctor