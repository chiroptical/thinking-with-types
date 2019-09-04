{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import           GHC.TypeLits

instance
  ( TypeError
    (      Text "Attempting to interpret a number as a function "
      :$$: Text "in the type `"
      :<>: ShowType (a -> b)
      :<>: Text "'"
      :$$: Text "Did you forget to specify the function you wanted?"
    )
  ) => Num (a -> b) where
