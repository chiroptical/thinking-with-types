{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module JsonSchema where

import Control.Monad.Writer
import Data.Aeson (Value (..), (.=), object)
import Data.Kind (Type)
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector (fromList)
import GHC.Generics
import GHC.TypeLits
import qualified GHC.TypeLits as Err

data Person =
  Person
    { name :: String
    , age :: Int
    , phone :: Maybe String 
    , permissions :: [Bool]
    } deriving Generic

class GSchema (a :: Type -> Type) where
  gschema :: Writer [Text] Value

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object $ a <> b

emitRequired :: forall nm. KnownSymbol nm => Writer [Text] ()
emitRequired = tell . pure . pack . symbolVal $ Proxy @nm

type family ToJsonType (a :: Type) :: Symbol where
  ToJsonType Int = "integer"
  ToJsonType Integer = "integer"
  ToJsonType Float = "number"
  ToJsonType Double = "number"
  ToJsonType String = "string"
  ToJsonType Bool = "boolean"
  ToJsonType [a] = "array"
  ToJsonType a = TypeName a

type family RepName (x :: Type -> Type) :: Symbol where
  RepName (D1 ('MetaData nm _ _ _) _) = nm

type family TypeName (t :: Type) :: Symbol where
  TypeName t = RepName (Rep t)

-- {"key": "value"}
makeTypeObj :: forall a. KnownSymbol (ToJsonType a) => Value
makeTypeObj = object ["type" .= String (pack . symbolVal $ Proxy @(ToJsonType a))]

makePropertyObj :: forall name. KnownSymbol name => Value -> Value
makePropertyObj v = object [pack (symbolVal $ Proxy @name) .= v]

instance (KnownSymbol nm, KnownSymbol (ToJsonType a)) =>
  GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 a)) where
  gschema = do
    emitRequired @nm
    pure . makePropertyObj @nm $ makeTypeObj @a
  {-# INLINE gschema #-}

instance (GSchema f, GSchema g) => GSchema (f :*: g) where
  gschema = mergeObjects <$> gschema @f <*> gschema @g
  {-# INLINE gschema #-}
