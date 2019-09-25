{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE RankNTypes #-}

module KanExtensions where

import Data.Functor.Yoneda
import Data.Functor.Day.Curried
import Control.Monad.Codensity

-- Rewriting too polymorphic code as Kan Extensions
-- 1. instead of `forall f. Functor f => f a`
--    use `forall f. Yoneda f a`
-- 2. instead of `forall f. Applicative f => f a`
--    use `forall f. Curried (Yoneda f) (Yoneda f) a`
-- 3. instead of `forall f. Monad f => f a`
--    use `forall f. Codensity f a`

-- newtype Yoneda f a =
--   Yoneda
--    { runYoneda :: forall b. (a -> b) -> f b
--    }

-- instance Functor (Yoneda f) where
--   fmap f (Yoneda y) = Yoneda (\k -> y (k . f))

-- newtype Codensity m a =
--   Codensity
--    { runCodensity :: forall b. (a -> m b) -> m b
--    }
