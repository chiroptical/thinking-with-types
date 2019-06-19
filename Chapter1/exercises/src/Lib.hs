module Lib where

import           Data.Word       (Word8)
import           Test.QuickCheck

-- Cardinality
-- ---
-- Given a data constructor, the amount of inhabitants
-- Syntax: |{data constuctor}| = {inhabitants}
--

-- |Void| = 0
data Void

-- |Unit| = 1
data Unit = Void

-- |Bool'| = 2
data Bool' = True' | False'

-- Isomorphic
-- ---
-- Any two data constructors with the same cardinality
-- are isomorphic, we can define the `to` and `from` functions
-- Syntax: $s \cong t$
-- Laws:
--   to . from = id
--   from . to = id

to :: s -> t
to = undefined

from :: t -> s
from = undefined

-- Bool and Spin are isomorphic

data Spin = Up | Down deriving (Eq, Show)

instance Arbitrary Spin where
  arbitrary = oneof [pure Up, pure Down]

toSpin :: Bool -> Spin
toSpin True  = Up
toSpin False = Down

fromSpin :: Spin -> Bool
fromSpin Up   = True
fromSpin Down = False

toSpin' :: Bool -> Spin
toSpin' True  = Down
toSpin' False = Up

fromSpin' :: Spin -> Bool
fromSpin' Up   = False
fromSpin' Down = True

-- Sum Types,
-- the cardinality is the sum of inhabitants' cardinality
-- Example:

data Either' a b = Left' a | Right' b
data Maybe' a = Nothing' | Just' a

-- Cardinality is |Either'| = |a| + |b|
-- |Either' Bool Bool| = |Bool| + |Bool| = 2 + 2 = 4
-- |Maybe' Bool| = |Nothing| + |Bool| = 1 + 2 = 3
-- Either' Bool Bool is not isomorphic with Maybe' Bool
-- However, Either' Unit Bool is isomorphic with Maybe' Bool

-- Product Types,
-- the cardinality is the product of the inhabitants cardinality
-- Example:

data MixedFraction a =
  Fraction
  { mixedBit    :: Word8
  , numerator   :: a
  , denominator :: a
  }

-- |MixedFraction Bool| = |Word8| x |Bool| x |Bool| = 256 x 2 x 2 = 1024

-- Prove `a x 1 = a` by showing an isomorphism between (a, ()) and a
-- |(a, b)| = |a| x |b|
-- |()| = 1
-- |(a, ()) = |a| x 1
-- therefore, |a| x 1 == |a|
prodUnitTo :: a -> (a, ())
prodUnitTo a = (a, ())

prodUnitFrom :: (a, ()) -> a
prodUnitFrom (a, _) = a

-- Functions,
-- the cardinality is computed as the result cardinality to the input cardinality power
-- |a -> b| = \prod_{i=0}^{|a|} |b_i| = |b|^{|a|}

-- Exercise: Determine the cardinality of Either Bool (Bool, Maybe Bool) -> Bool

-- |Bool| ^ (|Bool| + (|Bool| x (|Nothing| + |Bool|)))
-- 2 ^ (2 + (2 x (1 + 2)))
-- 2 ^ 8 = 256

data TicTacToe a = TicTacToe
  { topLeft      :: a
  , topCenter    :: a
  , topRight     :: a
  , centerLeft   :: a
  , centerCenter :: a
  , centerRight  :: a
  , bottomLeft   :: a
  , bottomCenter :: a
  , bottomRight  :: a
  }

emptyBoard :: TicTacToe (Maybe a)
emptyBoard = TicTacToe Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- |TicTacToe a| = |a|^9 == |a|^(3 x 3)
-- isomorphic to (|3|, |3|) -> a

data Three = One | Two | Three deriving (Eq, Ord, Enum, Bounded)

data TicTacToe' a = TicTacToe'
  { board :: Three -> Three -> a
  }

emptyBoard' :: TicTacToe' (Maybe a)
emptyBoard' = TicTacToe' $ const $ const Nothing

-- Algebra | Types
-- --------|-------------
-- a + b   | Either a b 
-- a x b   | (a, b)
-- b ^ a   | a -> b
-- a = b   | isomorphism
-- 0       | Void
-- 1       | Unit

-- Example, a ^ 1 = a
-- In Curry-Howard, represents an isomorphism between `() -> a` and `a`

-- Exercise, using Curry-Howard prove (a ^ b) ^ c is isomorphic
-- with a ^ (b x c), that is: provide a function of type
-- `(b -> c -> a) -> (b, c) -> a` and one of `((b, c) -> a) -> b -> c -> a`

-- a ^ (b x c) ~ (b, c) -> a
-- (a ^ b) ^ c ~ b -> c -> a

-- uncurry
fun :: (b -> c -> a) -> (b, c) -> a
fun f (y, z) = f y z

-- curry
fun' :: ((b, c) -> a) -> b -> c -> a
fun' f y z = f (y, z)

-- fun . fun' $ f == id f, fun' . fun $ f == id f

-- a ^ b x a ^ c = a ^ (b + c)
-- ---
-- a ^ b x a ^ c = (b -> a, c -> a)
-- a ^ (b + c) = Either b c -> a

exeTwoTo :: (b -> a, c -> a) -> (Either b c -> a)
exeTwoTo (f, _) (Left x) = f x
exeTwoTo (_, g) (Right y) = g y

exeTwoFrom :: (Either b c -> a) -> (b -> a, c -> a)
exeTwoFrom f = (f . Left, f . Right)