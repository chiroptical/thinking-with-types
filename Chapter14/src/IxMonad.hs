{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RebindableSyntax #-}

module IxMonad where

import Control.Monad.Indexed
import Data.Coerce
import Language.Haskell.DoNotation hiding (pure)
import Prelude hiding (Monad (..))

tupler :: Int -> (Int, Int)
tupler x = (x, x)

-- class IxApplicative m => IxMonad m where
--   ibind :: (a -> m j k b) -> m i j a -> m i k b

newtype Ix m i j a =
  Ix
    { unsafeRunIx :: m a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    )

instance Functor m => IxFunctor (Ix m) where
  imap = fmap

instance Applicative m => IxPointed (Ix m) where
  ireturn = pure

instance Applicative m => IxApplicative (Ix m) where
  iap :: forall i j k a b
        . Ix m i j (a -> b)
       -> Ix m j k a
       -> Ix m i k b
  iap = coerce $ (<*>) @m @a @b

instance Monad m => IxMonad (Ix m) where
  ibind :: forall i j k a b
        . (a -> Ix m j k b)
       -> Ix m i j a
       -> Ix m i k b
  ibind = coerce $ (=<<) @m @a @b
