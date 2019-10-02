{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WithSingleton where

import Data.Singletons.Prelude
import Data.Singletons.TH

-- class SDecide k where
--   (%~) :: Sing (a :: k)
--        -> Sing (b :: k)
--        -> Decision (a :~: b)

-- instance SDecide Bool where
--   STrue %~ STrue = Proved Refl
--   SFalse %~ SFalse = Proved Refl
--   _ %~ _ = Disproved $ const undefined

data Maybe' a =
    Nothing'
  | Just' a
  deriving (Eq, Ord, Show)

data instance Sing (a :: Maybe' k) where
  SJust' :: Sing (a :: k) -> Sing ('Just' a)
  SNothing' :: Sing 'Nothing'

instance SingI a => SingI ('Just' a) where
  sing = SJust' sing

instance SingI 'Nothing' where
  sing = SNothing'

-- Exercise 15.4-i Give instances of SDecide for Maybe
instance SDecide a => SDecide (Maybe' a) where
  SJust' a %~ SJust' a' =
    case a %~ a' of
      Proved Refl -> Proved Refl
      Disproved _ -> Disproved $ const undefined
  SNothing' %~ SNothing' = Proved Refl
  _ %~ _ = Disproved $ const undefined
