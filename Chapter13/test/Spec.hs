{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}

import JsonSchema

import Data.Aeson
import Test.Inspection

mySchema :: Value
mySchema = schema @Person

inspect $ hasNoGenerics 'mySchema

main :: IO ()
main = putStrLn "Test suite not yet implemented"
