{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Lib where

import Data.Coerce (Coercible (..), coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Sum (..), Product (..))

newtype ZipList a =
  ZipList
    { getZipList :: [a]
    }

-- newtype Sum' a =
--   Sum'
--     { getSum :: a
--     }

a = [54, 46]
b = [Sum 54, Sum 46]
c = ZipList [54, 46]
d = ZipList [Sum 54, Sum 46]

-- coerce :: Coercible a b => a -> b

-- O(n)
slowSum :: [Int] -> Int
-- mconcat = foldr (<>) mempty
slowSum = getSum . mconcat . fmap Sum

-- coerce creates O(0)
fastSum :: [Int] -> Int 
fastSum = getSum . mconcat . coerce

-- General rule, if you see `fmap NewtypeCtor`
-- it should be replaced with `coerce`

-- Laws of equality
-- 1. Reflexivity -- `Coercible a a` is true for any type
-- 2. Symmetry -- `Coercible a b` implies `Coercible b a`
-- 3. Transitivity -- given `Coercible a b` and `Coercible b c`
--                    we know `Coercible a c`

e = coerce (1867 :: Sum Int) :: Product Int

-- M.Map k v
-- insert :: Ord k => k -> v -> Map k v -> Map k v

newtype Reverse a =
  Reverse
    { getReverse :: a
    } deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
  compare (Reverse a) (Reverse b) = compare b a

-- Reverse a is safely Coercible with a
-- However, Map k v is not safely Coercible w/ Map (Reverse k) v

f = coerce (M.singleton 'S' True) :: M.Map Char (Reverse Bool)
--g = coerce (M.singleton 'S' True) :: M.Map (Reverse Char) Bool
--h = coerce (M.singleton 'S' True) :: M.Map Char Bool

-- k and v above have different roles, defined in
-- https://hackage.haskell.org/package/containers-0.6.2.1/docs/src/Data.Map.Internal.html#Map
-- type role Map nominal representational
--
-- nominal: the "everyday" notion of type-equality in Haskell
--          corresponding to `a ~ b` constraint. For example,
--          `Int` is only nominally equal to itself
-- representational: as discussed earlier in this chapter
--                   types `a` and `b` are representationally
--                   equal if and only if it's safe to reinterpret
--                   the memory of an `a` and `b`
-- phantom: two types are always phantom-ly equal to one another

-- Given `Sum a`, we say that `a` is at role representational
-- Coercible a b => Coercible (Sum a) (Sum b) that Sum a and Sum b
-- are representationally equal when a and b are representationally
-- equal

-- Coercible k1 k2 does NOT imply Coercible (Map k1 v) (Map k2 v)
-- because k must be at role nominal, only when k1 ~ k2 can the
-- two be coercible

-- data Proxy a = Proxy is role phantom because
-- Coercible (Proxy a) (Proxy b) is always true

i :: (a ~ b) => a -> b
i = id

j :: Int
j = 0

k :: Sum Int
k = Sum 0

l = j == coerce k

-- Inference Rules
-- 1. All type parameters are assumed phantom
-- 2. The type constructor (->) has two representational
--    roles; any type parameter applied to a (->) gets upgraded
--    to representational. Data constructors count as applying (->)
-- 3. The type constructor (~) has two nominal roles; any type
--    parameter applied to a (~) gets upgraded to nominal. GADTs
--    and type families count as applying (~)

-- Exercise 8.2-i
-- What is the role signature of Either a b?
-- type role Either representational representational
-- Exercise 8.2-ii
-- What is the role signature of Proxy a?
-- type role Proxy phantom

data BST v =
    Empty
  | Branch (BST v) v (BST v)

-- Inferred as:
-- type role BST representational
-- But, we could define (strengthening)
type role BST nominal
-- However, we can't define (weakening)
-- type role BST phantom
