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

-- a * 1 = a
prop_prodUnitTo :: Eq a => (a, ()) -> Bool
prop_prodUnitTo t = (prodUnitTo . prodUnitFrom $ t) == id t

prop_prodUnitFrom :: Eq a => a -> Bool
prop_prodUnitFrom x = (prodUnitFrom . prodUnitTo $ x) == id x

-- |Integer| ^ (|Integer| x |Integer|)
prop_curryIso :: Eq a => (b -> c -> a) -> b -> c -> Bool
prop_curryIso f a b = (fun' . fun $ f) a b == (id f) a b

prop_uncurryIso :: Eq a => ((b, c) -> a) -> b -> c -> Bool
prop_uncurryIso f a b = (fun . fun' $ f) (a, b) == (id f) (a, b)

prop_exeTwo :: Eq a => (Either b c -> a) -> Either b c -> Bool
prop_exeTwo f a = (exeTwoTo . exeTwoFrom $ f) a == (id f) a

prop_exeTwo' :: Eq a => (b -> a, c -> a) -> b -> c -> Bool
prop_exeTwo' f a b = g a == a' && h b == b'
  where (g, h) = exeTwoFrom . exeTwoTo $ f
        a' = fst (id f) $ a
        b' = snd (id f) $ b

instance Show (a -> b) where
  show _ = "a -> b"

prop_exeThree :: (Eq a, Eq b) => (c -> a, c -> b) -> c -> Bool
prop_exeThree f c = g c == a' && h c == b'
  where
    (g, h) = exeThreeTo . exeThreeFrom $ f
    a' = fst (id f) $ c
    b' = snd (id f) $ c

prop_exeThree' :: (Eq a, Eq b) => (c -> (a, b)) -> c -> Bool
prop_exeThree' f c = a == a' && b == b'
  where
    (a, b) = (id f) c
    (a', b') = (exeThreeFrom . exeThreeTo $ f) c

main :: IO ()
main = do
  quickCheck prop_spinIso
  quickCheck prop_boolIso
  quickCheck prop_spinIso'
  quickCheck prop_boolIso'
  quickCheck prop_spinNotIso
  quickCheck prop_boolNotIso
  quickCheck prop_spinNotIso'
  quickCheck prop_boolNotIso'
  quickCheck (prop_prodUnitTo :: (Integer, ()) -> Bool)
  quickCheck (prop_prodUnitFrom :: Integer -> Bool)
  quickCheck (prop_curryIso :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Bool)
  quickCheck (prop_uncurryIso :: ((Integer, Integer) -> Integer) -> Integer -> Integer -> Bool)
  quickCheck (prop_exeTwo :: (Either Integer Integer -> Integer) -> Either Integer Integer -> Bool)
  quickCheck (prop_exeTwo' :: ((Integer -> Integer), (Integer -> Integer)) -> Integer -> Integer -> Bool)
  quickCheck (prop_exeThree :: (Integer -> Integer, Integer -> Integer) -> Integer -> Bool)
  quickCheck (prop_exeThree' :: (Integer -> (Integer, Integer)) -> Integer -> Bool)