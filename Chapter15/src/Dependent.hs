{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Dependent where

import Data.Aeson
import Data.Maybe (mapMaybe)
import Data.Constraint
import Data.Kind (Type)
import Data.Singletons.Prelude
import Data.Singletons.TH

data Sigma (f :: k -> Type) where
  Sigma :: Sing a -> f a -> Sigma f

withSigma
  :: (forall (a :: k). Sing a -> f a -> r)
  -> Sigma f
  -> r
withSigma c (Sigma s f) = c s f

toSigma
  :: SingI a
  => f a
  -> Sigma f
toSigma = Sigma sing

fromSigma
  :: forall k (a :: k) (f :: k -> Type).
    ( SingI a
    , SDecide k
    )
  => Sigma f
  -> Maybe (f a)
-- think of `s` below as `Sing t`
-- -> If `Sing t` ~ `Sing a` then `f a` ~ `f t`
fromSigma (Sigma s f) =
  case s %~ sing @a of
    Proved Refl -> Just f
    Disproved _ -> Nothing

class Dict1 c (f :: k -> Type) where
  dict1 :: Sing (a :: k) -> Dict (c (f a))

instance ( Dict1 Eq (f :: k -> Type)
         , SDecide k
         ) => Eq (Sigma f) where
  Sigma sa fa == Sigma sb fb =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @Eq @f sa of
          -- By being able to pattern match on `Dict` the constraint `Eq (f t)`
          -- and its implementation are available at compile time
          -- -> Keeping in mind that both `fa :: f t` and `fb :: f t`
          Dict -> fa == fb
      Disproved _ -> False

instance ( Dict1 Show (f :: k -> Type)
         , Show (Demote k)
         , SingKind k
         ) => Show (Sigma f) where
  show (Sigma sa fa) =
    case dict1 @Show @f sa of
      Dict -> mconcat
        [ "Sigma "
        , show $ fromSing sa
        , " ("
        , show fa
        , ")"
        ]

-- Exercise 15.5-i: Provide an instance of `Ord` for `Sigma`
-- by comparing the `fs` if the singletons are equal, comparing
-- the singletons at the term level otherwise
instance ( Dict1 Eq (f :: k -> Type)
         , Dict1 Ord f
         , SDecide k
         , Ord (Demote k)
         , SingKind k
         ) => Ord (Sigma f) where
  compare (Sigma sa fa) (Sigma sb fb) =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @Ord @f sa of
          -- By being able to pattern match on `Dict` the constraint `Eq (f t)`
          -- and its implementation are available at compile time
          -- -> Keeping in mind that both `fa :: f t` and `fb :: f t`
          Dict -> compare fa fb
      Disproved _ -> compare (fromSing sa) (fromSing sb)

-- Section 15.5.1 Structured Logging

singletons [d|
  data LogType
    = JsonMsg
    | TextMsg
    deriving (Eq, Ord, Show)
  |]

data family LogMsg (msg :: LogType)

data instance LogMsg 'JsonMsg
  = Json Value
  deriving (Eq, Show)
  
data instance LogMsg 'TextMsg
  = Text String
  deriving (Eq, Show)

instance ( c (LogMsg 'JsonMsg)
         , c (LogMsg 'TextMsg)
         ) => Dict1 c LogMsg where
  dict1 SJsonMsg = Dict
  dict1 STextMsg = Dict

logs :: [Sigma LogMsg]
logs =
  [ toSigma $ Text "hello"
  , toSigma $ Json $
    object [ "world" .= (5 :: Int) ]
  , toSigma $ Text "Structured logging is super green"
  ]

showLogs :: [Sigma LogMsg] -> [String]
showLogs = fmap $ withSigma $ \sa fa ->
  case dict1 @Show @LogMsg sa of
    Dict -> show fa

catSigmas
  :: forall k (a :: k) f
  .  (SingI a, SDecide k)
  => [Sigma f]
  -> [f a]
catSigmas = mapMaybe fromSigma

jsonLogs :: [LogMsg 'JsonMsg]
jsonLogs = catSigmas logs
