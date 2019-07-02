{-# LANGUAGE RankNTypes #-}

module Lib where

-- rank 0
five :: Int -> Int
five = (+5)

-- A broken rank 1 function
-- applyToFive :: (a -> a) -> Int
-- applyToFive f = f 5

-- A working rank 2 function
applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

-- foo :: forall r. ((forall a. a -> r) -> r)
-- This type signature is impossible to implement in GHC
-- -> it is called an impredicative type

-- What is the rank of the following type signatures

a :: Int -> (forall a. a -> a)
a = undefined
-- rank 1

-- a -> b -> c is actually parsed a -> (b -> c)
b :: (a -> b) -> (forall c. c -> (a -> b))
b f = undefined
-- rank 2

c :: ((forall x. m x -> b (z m x)) -> b (z m a)) -> m a
c f = undefined
-- rank 3

-- The types `a` and `forall r. (a -> r) -> r` are isomorphic
cont :: a -> (forall r. (a -> r) -> r)
cont a = \cb -> cb a

runCont :: (forall r. (a -> r) -> r) -> a
runCont = \f -> f id

-- the forall r. (a -> r) -> r is known as continuation-passing style (CPR)

-- Question: How do we know this isomorphism is true without testing it?
-- Do cont . runCont == id and runCont . cont == id?

compose :: (forall r. (a -> r) -> r) -> (forall r. (a -> r) -> r)
compose = \x -> cont (runCont x)

-- compose :: Eq a => (forall r. (a -> r) -> r) -> Bool
-- compose x = f id == x id
--   where f = cont $ runCont x

compose' :: a -> a
compose' = \x -> runCont (cont x)

-- compose' :: Eq a => a -> Bool
-- compose' x = (runCont $ cont x) == id x

-- \x -> runCont (cont x)
-- \x -> runCont (\cb -> cb x)
-- \x -> (\cb -> cb x) id
-- \x -> id x
-- \x -> x

-- \x -> cont (runCont x)
-- \x -> cont ((\f -> f id) x)
-- \x -> cont (x id)
-- \x -> (\cb -> cb (x id))

newtype Cont a =
  Cont
    { unCont :: forall r. (a -> r) -> r
    }

-- Need to compare these carefully to the answers provided in the book

instance Functor Cont where
  fmap f (Cont arr) = Cont $ \br -> br $ arr f

instance Applicative Cont where
  pure a = Cont $ \ar -> ar a
  Cont fabrr <*> Cont arr = Cont $ \br -> br $ fabrr arr

instance Monad Cont where
  return = pure
  Cont arr >>= faCb = arr faCb

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOs :: (String -> r) -> r
withOs f = f "linux"

releaseString :: String
releaseString =
  withVersionNumber $ \version ->
    withTimestamp $ \date ->
      withOs $ \os ->
        os ++ "-" ++ show version ++ "-" ++ show date

releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
  version <- Cont withVersionNumber
  date <- Cont withTimestamp
  os <- Cont withOs
  return $ os ++ "-" ++ show version ++ "-" ++ show date

newtype ContT m a =
  ContT
    { runContT :: forall r. (a -> m r) -> m r
    }

-- How do these work?

instance Functor (ContT m) where
  fmap f (ContT marr) = ContT $ \br -> marr (br . f)

instance Applicative (ContT m) where
  pure x = ContT $ \ar -> ar x
  ContT fabrr <*> ContT farr =
    ContT $ \br -> fabrr $
      \ab -> farr (br . ab)

instance Monad (ContT m) where
  return = pure
  ContT arr >>= faCb =
    ContT $ \c -> arr $
     \a -> runContT (faCb a) c
