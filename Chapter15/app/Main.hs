{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Lib
import Data.Constraint (Dict (..))

program :: MonadLogging b => LoggingMonad b ()
program = do
  logMsg "beep bop beep boop, logging robot here!"
  pure ()

main :: IO ()
main = do
  bool <- read <$> getLine 
  withSomeSBool (toSBool bool) $
    \(sb :: SBool b) ->
      case dict @MonadLogging sb of
        Dict -> runLogging @b program
