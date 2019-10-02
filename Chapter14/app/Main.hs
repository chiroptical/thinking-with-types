{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Indexed
import Data.Coerce
import Fcf
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import Language.Haskell.DoNotation hiding (pure)
import Prelude hiding (Monad (..))
import qualified System.IO as SIO
import System.IO hiding (openFile, Handle)

import IxMonad

data LinearState =
  LinearState
    { linearNextKey :: Nat
    , linearOpenKeys :: [Nat]
    }

newtype Linear s (i :: LinearState) (j :: LinearState) a =
  Linear
    { unsafeRunLinear :: Ix IO i j a
    }
  deriving
    ( IxFunctor
    , IxPointed
    , IxApplicative
    , IxMonad
    )

openFile
  :: FilePath
  -> IOMode
  -> Linear s ('LinearState next open)
              ('LinearState
                (next TL.+ 1)
                (next ': open))
              (Handle s next)
openFile = coerce SIO.openFile

newtype Handle s key =
  Handle
    { unsafeGetHandle :: SIO.Handle
    }

type IsOpen (key :: k) (ts :: [k]) =
  IsJust =<< Find (TyEq key) ts

type Close (key :: k) (ts :: [k]) =
  Filter (Not <=< TyEq key) ts

closeFile
  :: Eval (IsOpen key open) ~ 'True
  => Handle s key
  -> Linear s ('LinearState next open)
              ('LinearState next (Eval (Close key open)))
              ()
closeFile = coerce SIO.hClose

runLinear
  :: ( forall s
     . Linear s ('LinearState 0 '[])
                ('LinearState n '[]) a
     )
  -> IO a
runLinear = coerce

main :: IO ()
main = putStrLn "Indexed Monads"
