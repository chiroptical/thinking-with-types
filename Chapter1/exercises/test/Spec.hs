{-# LANGUAGE FlexibleInstances #-}
module Main where

import Test.QuickCheck
import Lib

prop_spinIso :: Spin -> Bool
prop_spinIso s = (toSpin . fromSpin $ s) == id s

prop_boolIso :: Bool -> Bool
prop_boolIso b = (fromSpin . toSpin $ b) == id b

prop_spinIso' :: Spin -> Bool
prop_spinIso' s = (toSpin' . fromSpin' $ s) == id s

prop_boolIso' :: Bool -> Bool
prop_boolIso' b = (fromSpin' . toSpin' $ b) == id b

prop_spinNotIso :: Spin -> Bool
prop_spinNotIso s = (toSpin' . fromSpin $ s) /= id s

prop_boolNotIso :: Bool -> Bool
prop_boolNotIso b = (fromSpin' . toSpin $ b) /= id b

prop_spinNotIso' :: Spin -> Bool
prop_spinNotIso' s = (toSpin . fromSpin' $ s) /= id s

prop_boolNotIso' :: Bool -> Bool
prop_boolNotIso' b = (fromSpin . toSpin' $ b) /= id b

-- |Integer| ^ (|Integer| x |Integer|)
prop_curryIso :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Bool
prop_curryIso f a b = (fun' . fun $ f) a b == (id f) a b

prop_uncurryIso :: ((Integer, Integer) -> Integer) -> Integer -> Integer -> Bool
prop_uncurryIso f a b = (fun . fun' $ f) (a, b) == (id f) (a, b)

instance Show (Integer -> Integer -> Integer) where
  show _ = "I -> I -> I"

instance Show ((Integer, Integer) -> Integer) where
  show _ = "(I, I) -> I"

prop_exeTwo :: (Either Integer Integer -> Integer) -> Either Integer Integer -> Bool
prop_exeTwo f a = (exeTwoTo . exeTwoFrom $ f) a == (id f) a

prop_exeTwo' :: (Integer -> Integer, Integer -> Integer) -> Integer -> Integer -> Bool
prop_exeTwo' f a b = (g a == a') && (h b == b')
  where (g, h) = exeTwoFrom . exeTwoTo $ f
        a' = fst (id f) $ a
        b' = snd (id f) $ b

instance Show (Either Integer Integer -> Integer) where
  show _ = "Either I I -> I"

instance Show (Integer -> Integer) where
  show _ = "I -> I"

main :: IO ()
main = do
  -- quickCheck prop_spinIso
  -- quickCheck prop_boolIso
  -- quickCheck prop_spinIso'
  -- quickCheck prop_boolIso'
  -- quickCheck prop_spinNotIso
  -- quickCheck prop_boolNotIso
  -- quickCheck prop_spinNotIso'
  -- quickCheck prop_boolNotIso'
  -- quickCheck prop_curryIso
  -- quickCheck prop_uncurryIso
  quickCheck prop_exeTwo
  quickCheck prop_exeTwo'