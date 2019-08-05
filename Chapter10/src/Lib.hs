{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}

module Lib where

import           Prelude                 hiding ( fst )
import           Data.Kind                      ( Constraint
                                                , Type
                                                )

fst :: (a, b) -> a
fst (x, _) = x

newtype Fst a b =
  Fst { unFst :: (a, b)
      }

-- Syntax `| l -> t` means `t` is fully determined by `l`
class Eval l t | l -> t where
  eval :: l -> t

instance Eval (Fst a b) a where
  eval (Fst (x, y)) = x

-- Exercise 10.1-i: Defunctionalize listToMaybe
listToMaybe :: [a] -> Maybe a
listToMaybe [x] = Just x
listToMaybe _   = Nothing

newtype ListToMaybe a =
  ListToMaybe
    { unListToMaybe :: [a]
    }

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe [x]) = Just x
  eval _                 = Nothing

data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList f []      ) = []
  eval (MapList f (x : xs)) = eval (f x) : eval (MapList f xs)
