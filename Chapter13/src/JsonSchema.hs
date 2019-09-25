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
import Fcf

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
  -- Experimenting with the idea of writing maybe types prefaced with "maybe "
  --ToJsonType (Maybe a) = AppendSymbol "maybe " (ToJsonType a)
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

instance (TypeError ('Err.Text "Json Schema does not support sum types ...")) => GSchema (f :+: g) where
  gschema = error "Json Schema does not support sum types"
  {-# INLINE gschema #-}

instance GSchema a => GSchema (M1 C _1 a) where
  gschema = gschema @a
  {-# INLINE gschema #-}

instance (GSchema a, KnownSymbol nm)
    => GSchema (M1 D ('MetaData nm _1 _2 _3) a) where
  gschema = do
    sch <- gschema @a
    pure $ object
      [ "title" .= (String . pack . symbolVal $ Proxy @nm)
      , "type" .= String "object"
      , "properties" .= sch
      ]
  {-# INLINE gschema #-}

schema :: forall a. (GSchema (Rep a), Generic a) => Value
schema =
  let (v, reqs) = runWriter $ gschema @(Rep a)
   in mergeObjects v $ object
    [ "required" .= Array (fromList $ String <$> reqs)
    ]
{-# INLINE schema #-}

instance {-# OVERLAPPING #-}
  ( KnownSymbol nm
  , KnownSymbol (ToJsonType a)
  )
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3)
                     (K1 _4 (Maybe a))) where 
  gschema = pure . makePropertyObj @nm $ makeTypeObj @a
  {-# INLINE gschema #-}

instance {-# OVERLAPPING #-}
  ( KnownSymbol nm
  , KnownSymbol (ToJsonType [a])
  , KnownSymbol (ToJsonType a)
  )
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3)
                     (K1 _4 [a])) where
  gschema = do
    emitRequired @nm
    let innerType = object ["items" .= makeTypeObj @a]
    pure . makePropertyObj @nm . mergeObjects innerType $ makeTypeObj @[a]
  {-# INLINE gschema #-}

instance {-# OVERLAPPING #-}
  ( KnownSymbol nm
  )
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3)
                     (K1 _4 String)) where
  gschema = do
    emitRequired @nm
    pure . makePropertyObj @nm $ makeTypeObj @String
  {-# INLINE gschema #-}
