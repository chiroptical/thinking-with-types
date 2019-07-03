{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import           Data.Foldable
import           Data.IORef
import           Data.Kind
import           Data.Maybe
import           Data.Typeable
import           System.IO.Unsafe (unsafePerformIO)

-- These Any types can contain anything
-- The type system forgets what `a` is

-- data Any = forall a. Any a

data Any where
  Any :: a -> Any

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

-- Are functions of the type (forall a. a -> r)
-- interesting? Why or why not?

-- This function is the CPS. Because of the polymorphism
-- of `a`, `r` can only ever be a constant

data HasShow where
  HasShow :: Show t => t -> HasShow

-- instance Show HasShow where
--   show (HasShow s) = "HasShow " ++ show s

elimHasShow
  :: (forall a. Show a => a -> r)
  -> HasShow
  -> r
elimHasShow f (HasShow a) = f a

instance Show HasShow where
  show hst = elimHasShow show hst

data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic
  :: (forall a. Typeable a => a -> r)
  -> Dynamic
  -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2
  :: forall a b r.
    ( Typeable a
    , Typeable b
    , Typeable r
    )
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "TypeError: unsupported operand type(s) for +") $ asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int  @Int  a b (+)
    , liftD2 @String @Int  a b $ \strA intB -> strA ++ show intB
    , liftD2 @Int  @String a b $ \intA strB -> show intA ++ strB
    , liftD2 @[Int] @[Int] a b (++)
    ]

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas
  :: (forall a. c a => a -> r)
  -> Has c
  -> r
elimHas f (Has a) = f a

type HasShow' = Has Show
type Dynamic' = Has Typeable

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty = (== mempty)

-- known as _constraint synonym_
-- requires: AllowAmbiguousTypes, UndecidableInstances
class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a

newtype ST s a =
  ST
    { unsafeRunST :: a
    }
  
instance Functor (ST s) where
  fmap f (ST a) = seq a (ST $ f a)

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f . seq a $ (ST $ f a)

instance Monad (ST s) where
  ST a >>= f = seq a $ f a

newtype STRef s a =
  STRef 
    { unSTRef :: IORef a
    }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  a <- readSTRef ref
  writeSTRef ref $ f a

runST
  :: (forall s. ST s a)
  -> a
runST = unsafeRunST

safeExample :: ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTRef ref (++ " world")
  readSTRef ref

-- runST
--   :: (forall s. ST s (STRef s Bool))
--   -> STRef s Bool