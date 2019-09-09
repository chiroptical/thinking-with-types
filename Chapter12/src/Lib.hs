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

-- This will never compile! It will always emit the type level
-- error!
-- foo :: TypeError (Text "This should trigger always, yes?") => a
-- foo = undefined
