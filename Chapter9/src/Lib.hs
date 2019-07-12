{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

-- "%c%d%d"
-- Char -> Int -> Int -> String
--
-- "some number: %d"
-- Int -> String
-- \s -> "show number: " <> show s
import           Data.Kind                      ( Type )
import           Data.Monoid                    ( (<>) )
import           Data.Proxy                     ( Proxy(..) )
import           GHC.TypeLits

-- Referring to this as "printf operator"
data (a :: k1) :<< (b :: k2)

infixr 5 :<<

-- Given Int :<< ":" :<< Bool :<< "!" we'd like to
-- produce a function :: Int -> Bool -> String
class HasPrintf a where
  type Printf a :: Type
  format :: String -> Proxy a -> Printf a

-- 1. HasPrintf (text :: Symbol)
-- 2. HasPrintf a => HasPrintf ((text :: Symbol) :<< a)
-- 3. HasPrintf a => HasPrintf ((param :: Type) :<< a)
instance KnownSymbol text => HasPrintf (text :: Symbol) where
    type Printf text = String
    format s _ = s <> symbolVal (Proxy @text)

instance (HasPrintf a, KnownSymbol text) =>
         HasPrintf ((text :: Symbol) :<< a) where
    type Printf (text :<< a) = Printf a
    format s _ = format (s <> symbolVal (Proxy @text)) (Proxy @a)

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
    type Printf (param :<< a) = param -> Printf a
    format s _ param = format (s <> show param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""

instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
    type Printf (String :<< a) = String -> Printf a
    format s _ param = format (s <> param) (Proxy @a)
-- Would require -XAllowAmbiguousTypes
--printf' :: forall a. HasPrintf a => Printf a
--printf' = printf (Proxy @a)
