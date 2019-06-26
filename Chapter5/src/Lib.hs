{-# LANGUAGE GADTs #-}
module Lib where

a :: Show a => a -> String
a x = show x

b = 1 == 1

five :: Int
five = 5

five' :: (a ~ Int) => a
five' = 5

-- (a ~ b) is called a type equality

-- Type equalities form an equivalence relation, meaning
-- they have the following properties
-- 1. _reflexivity_: a type is always equal to itself, i.e. (a ~ a)
-- 2. _symmetry_: (a ~ b) holds, only if (b ~ a) 
-- 3. _transitivity_: if both (a ~ b) and (b ~ c) then we can infer (a ~ c)

data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt i) = i
evalExpr (LitBool b) = b
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Not x) = not $ evalExpr x
evalExpr (If b x y) =
  if evalExpr b
   then evalExpr x
   else evalExpr y

data Expr' a =
    (a ~ Int) => LitInt' Int
  | (a ~ Bool) => LitBool' Bool
-- | ...  